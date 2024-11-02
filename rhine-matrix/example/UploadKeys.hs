{-# LANGUAGE DuplicateRecordFields #-}

module UploadKeys where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.RFC8785
import Data.Aeson.Types (listValue)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.Matrix.Client
import Vodozemac qualified
import Prelude
import Debug.Trace (traceShowM)

newtype SignedObject = SignedObject Object
    deriving newtype (ToJSON)

{-
  "signatures": {
    "@alice:example.com": {
      "ed25519:JLAFKJWSCS": "FLWxXqGbwrb8SM3Y795eB6OA8bwBcoMZFXBqnTn58AYWZSqiD45tlBVcDa2L7RwdKXebW/VzDlnfVJ+9jok1Bw"
    }
  }
 -}
signObject :: Vodozemac.Account -> UserID -> DeviceID -> Object -> IO SignedObject
signObject account (UserID userId) (DeviceID deviceId) obj = do
    let objStr = LazyByteString.toStrict $ encodeCanonical obj
    signature <- Vodozemac.ed25519SignatureToBase64 =<< Vodozemac.sign account objStr
    let signatures = object [Key.fromText userId .= ed25519]
        ed25519 = object [("ed25519:" <> Key.fromText deviceId) .= signature]
    pure . SignedObject $ KeyMap.insert "signatures" signatures obj

data DeviceKeys = DeviceKeys
    { userId :: UserID
    , deviceId :: DeviceID
    , curve25519Key :: Text
    , ed25519Key :: Text
    }

instance ToJSON DeviceKeys where
    toJSON DeviceKeys{userId = UserID userId, deviceId = DeviceID deviceId, ..} =
        object
            [ "algorithms" .= listValue @Text toJSON ["m.olm.v1.curve25519-aes-sha2", "m.megolm.v1.aes-sha2"]
            , "device_id" .= toJSON deviceId
            , "keys"
                .= object
                    [ ("curve25519:" <> Key.fromText deviceId) .= curve25519Key
                    , ("ed25519:" <> Key.fromText deviceId) .= ed25519Key
                    ]
            , "user_id" .= toJSON userId
            ]

data KeyUploadRequest = KeyUploadRequest
    { deviceKeys :: DeviceKeys
    , fallbackKeyId :: Text
    , fallbackKey :: Text
    }

data SignedKeyUploadRequest = SignedKeyUploadRequest
    { signedDeviceKeys :: SignedObject
    , signedFallbackKey :: Object
    }

instance ToJSON SignedKeyUploadRequest where
    toJSON SignedKeyUploadRequest{..} =
        object ["device_keys" .= toJSON signedDeviceKeys, "fallback_keys" .= toJSON signedFallbackKey]

signKeyUploadRequest :: Vodozemac.Account -> KeyUploadRequest -> IO SignedKeyUploadRequest
signKeyUploadRequest account KeyUploadRequest{..} = do
    Object deviceKeysObj <- pure $ toJSON deviceKeys
    signedDeviceKeys <- signObject account deviceKeys.userId deviceKeys.deviceId deviceKeysObj
    signedFallbackKey' <- signObject account deviceKeys.userId deviceKeys.deviceId $
        KeyMap.fromList [("fallback", toJSON True), ("key", toJSON fallbackKey)]
    let signedFallbackKey = KeyMap.singleton ("signed_curve25519:" <> Key.fromText fallbackKeyId) (toJSON signedFallbackKey')
    pure SignedKeyUploadRequest{..}

{-
{
  "device_keys": {
    "algorithms": [
      "m.olm.v1.curve25519-aes-sha2",
      "m.megolm.v1.aes-sha2"
    ],
    "device_id": "JLAFKJWSCS",
    "keys": {
      "curve25519:JLAFKJWSCS": "3C5BFWi2Y8MaVvjM8M22DBmh24PmgR0nPvJOIArzgyI",
      "ed25519:JLAFKJWSCS": "lEuiRJBit0IG6nUf5pUzWTUEsRVVe/HJkoKuEww9ULI"
    },
    "signatures": {
      "@alice:example.com": {
        "ed25519:JLAFKJWSCS": "dSO80A01XiigH3uBiDVx/EjzaoycHcjq9lfQX0uWsqxl2giMIiSPR8a4d291W1ihKJL/a+myXS367WT6NAIcBA"
      }
    },
    "user_id": "@alice:example.com"
  },
  "fallback_keys": {
    "signed_curve25519:AAAAGj": {
      "fallback": true,
      "key": "zKbLg+NrIjpnagy+pIY6uPL4ZwEG2v+8F9lmgsnlZzs",
      "signatures": {
        "@alice:example.com": {
          "ed25519:JLAFKJWSCS": "FLWxXqGbwrb8SM3Y795eB6OA8bwBcoMZFXBqnTn58AYWZSqiD45tlBVcDa2L7RwdKXebW/VzDlnfVJ+9jok1Bw"
        }
      }
    }
  },
  "one_time_keys": {
    "signed_curve25519:AAAAHQ": {
      "key": "j3fR3HemM16M7CWhoI4Sk5ZsdmdfQHsKL1xuSft6MSw",
      "signatures": {
        "@alice:example.com": {
          "ed25519:JLAFKJWSCS": "IQeCEPb9HFk217cU9kw9EOiusC6kMIkoIRnbnfOh5Oc63S1ghgyjShBGpu34blQomoalCyXWyhaaT3MrLZYQAA"
        }
      }
    },
    "signed_curve25519:AAAAHg": {
      "key": "zKbLg+NrIjpnagy+pIY6uPL4ZwEG2v+8F9lmgsnlZzs",
      "signatures": {
        "@alice:example.com": {
          "ed25519:JLAFKJWSCS": "FLWxXqGbwrb8SM3Y795eB6OA8bwBcoMZFXBqnTn58AYWZSqiD45tlBVcDa2L7RwdKXebW/VzDlnfVJ+9jok1Bw"
        }
      }
    }
  }
}
-}

uploadKeys :: ClientSession -> Vodozemac.Account -> UserID -> DeviceID -> MatrixIO ()
uploadKeys session account userId deviceId = do
    Just (Vodozemac.FallbackKey fallbackKeyId' fallbackKey') <- Vodozemac.fallbackKey account
    fallbackKeyId <- Text.pack <$> Vodozemac.keyIdToBase64 fallbackKeyId'
    fallbackKey <- Text.pack <$> Vodozemac.curve25519PublicKeyToBase64 fallbackKey'
    curve25519Key <- Text.pack <$> (Vodozemac.curve25519PublicKeyToBase64 . fromJust =<< Vodozemac.curve25519Key account)
    ed25519Key <- Text.pack <$> (Vodozemac.ed25519PublicKeyToBase64 . fromJust =<< Vodozemac.ed25519Key account)
    signedKeys <- signKeyUploadRequest account KeyUploadRequest{deviceKeys = DeviceKeys{..}, ..}
    traceShowM $ encode signedKeys
    request <- mkRequest session True "/_matrix/client/v3/keys/upload"
    doRequest session request{HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ encodeCanonical signedKeys}

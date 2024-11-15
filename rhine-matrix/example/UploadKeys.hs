{-# LANGUAGE DuplicateRecordFields #-}

module UploadKeys where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.RFC8785
import Data.Aeson.Types (listValue)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Debug.Trace (traceShowM)
import Network.HTTP.Client qualified as HTTP
import Network.Matrix.Client
import Vodozemac.Account qualified
import Vodozemac.Account qualified as Vodozemac (Account)
import Vodozemac.Curve25519PublicKey qualified
import Vodozemac.Ed25519PublicKey qualified
import Vodozemac.Ed25519Signature qualified
import Vodozemac.FallbackKey qualified as Vodozemac (FallbackKey (..))
import Vodozemac.KeyId qualified
import Prelude

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
    signature <- Vodozemac.Ed25519Signature.toBase64 =<< Vodozemac.Account.sign account objStr
    let signatures = object [Key.fromText userId .= ed25519]
        ed25519 = object [("ed25519:" <> Key.fromText deviceId) .= Text.decodeUtf8 signature]
    pure . SignedObject $ KeyMap.insert "signatures" signatures obj

data DeviceKeys = DeviceKeys
    { userId :: UserID
    , deviceId :: DeviceID
    , curve25519Key :: ByteString
    , ed25519Key :: ByteString
    }

instance ToJSON DeviceKeys where
    toJSON DeviceKeys{userId = UserID userId, deviceId = DeviceID deviceId, ..} =
        object
            [ "algorithms" .= listValue @Text toJSON ["m.olm.v1.curve25519-aes-sha2", "m.megolm.v1.aes-sha2"]
            , "device_id" .= toJSON deviceId
            , "keys"
                .= object
                    [ ("curve25519:" <> Key.fromText deviceId) .= Text.decodeUtf8 curve25519Key
                    , ("ed25519:" <> Key.fromText deviceId) .= Text.decodeUtf8 ed25519Key
                    ]
            , "user_id" .= toJSON userId
            ]

data KeyUploadRequest = KeyUploadRequest
    { deviceKeys :: DeviceKeys
    , fallbackKeyId :: ByteString
    , fallbackKey :: ByteString
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
    signedFallbackKey' <-
        signObject account deviceKeys.userId deviceKeys.deviceId $
            KeyMap.fromList [("key", toJSON (Text.decodeUtf8 fallbackKey))]
    -- This should really be `KeyMap.fromList [("fallback", toJSON True), ("key", toJSON (Text.decodeUtf8 fallbackKey))]`
    -- But ... certain clients seem to not understand that they might get a fallback key from /keys/claim and then start complaining about not being able to verify the signature for the key
    -- Just leaving out the `fallback` marker seems to be good enough for clients that support it as well, so this should keep us compatible enough.
    let signedFallbackKey = KeyMap.singleton ("signed_curve25519:" <> Key.fromText (Text.decodeUtf8 fallbackKeyId)) (toJSON signedFallbackKey')
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
    Just (Vodozemac.FallbackKey fallbackKeyId' fallbackKey') <- Vodozemac.Account.fallbackKey account
    fallbackKeyId <- Vodozemac.KeyId.toBase64 fallbackKeyId'
    fallbackKey <- Vodozemac.Curve25519PublicKey.toBase64 fallbackKey'
    curve25519Key <- Vodozemac.Curve25519PublicKey.toBase64 =<< Vodozemac.Account.curve25519Key account
    ed25519Key <- Vodozemac.Ed25519PublicKey.toBase64 =<< Vodozemac.Account.ed25519Key account
    signedKeys <- signKeyUploadRequest account KeyUploadRequest{deviceKeys = DeviceKeys{..}, ..}
    traceShowM $ encode signedKeys
    request <- mkRequest session True "/_matrix/client/v3/keys/upload"
    doRequest session request{HTTP.method = "POST", HTTP.requestBody = HTTP.RequestBodyLBS $ encodeCanonical signedKeys}

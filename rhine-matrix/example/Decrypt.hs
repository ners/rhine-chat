{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Decrypt where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import Network.Matrix.Client hiding (RoomEvent)
import Vodozemac.Account qualified
import Vodozemac.Account qualified as Vodozemac (Account)
import Vodozemac.Curve25519PublicKey qualified
import Vodozemac.Megolm.InboundGroupSession qualified
import Vodozemac.Megolm.InboundGroupSession qualified as Vodozemac.Megolm (InboundGroupSession)
import Vodozemac.Megolm.SessionKey (SessionKey)
import Vodozemac.Megolm.SessionKey qualified
import Prelude

aesonOptions :: Aeson.Options
aesonOptions = (aesonDrop 0 snakeCase){Aeson.omitNothingFields = True}

data SessionKeyContent = SessionKeyContent
    { roomId :: Text
    , sessionId :: Text
    , sessionKey :: Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON SessionKeyContent where
    parseJSON = Aeson.genericParseJSON aesonOptions

instance Aeson.ToJSON SessionKeyContent where
    toJSON = Aeson.genericToJSON aesonOptions

data RoomKeyMessage = RoomKeyMessage
    { content :: SessionKeyContent
    , keys :: Map Text Text
    , recipient :: Text
    , recipientKeys :: Map Text Text
    , sender :: Text
    , senderDevice :: Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON RoomKeyMessage where
    parseJSON = Aeson.withObject "RoomKeyMessage" \o -> do
        type' :: String <- o Aeson..: "type"
        case type' of
            "m.room_key" -> Aeson.genericParseJSON aesonOptions (Aeson.toJSON o)
            _ -> mzero

instance Aeson.ToJSON RoomKeyMessage where
    toJSON rkm =
        case Aeson.genericToJSON aesonOptions rkm of
            Aeson.Object o -> Aeson.Object $ Aeson.KeyMap.insert "type" (Aeson.toJSON @String "m.room_key") o
            x -> x

decryptDeviceEvent :: Vodozemac.Account -> Text -> MRoomEncryptedPayload -> IO (Maybe SessionKey)
decryptDeviceEvent account senderKeyBase64 payload = do
    senderKey <- Vodozemac.Curve25519PublicKey.fromBase64 (Text.encodeUtf8 senderKeyBase64)
    Aeson.Object ciphertext <- pure $ Aeson.toJSON payload
    Vodozemac.Account.createInboundSession account senderKey ciphertext >>= \case
        Nothing -> do
            print $ "Failed to decrypt device event: " <> show payload
            pure Nothing
        Just (_session, Aeson.decodeStrict @RoomKeyMessage -> Just roomKeyMessage) -> do
            putStrLn "Decrypted room key message:"
            Text.putStrLn . Text.decodeUtf8 . LazyByteString.toStrict $ Aeson.encodePretty roomKeyMessage
            sessionKey <- Vodozemac.Megolm.SessionKey.fromBase64 $ Text.encodeUtf8 roomKeyMessage.content.sessionKey
            pure . pure $ sessionKey
        Just (_session, wat) -> do
            print $ "Failed to decode device event: " <> show wat
            pure Nothing

data MegolmContent = MegolmContent
    { ciphertext :: Text
    , deviceId :: Text
    , senderKey :: Text
    , sessionId :: Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON MegolmContent where
    parseJSON = Aeson.genericParseJSON aesonOptions

instance Aeson.ToJSON MegolmContent where
    toJSON = Aeson.genericToJSON aesonOptions

decryptRoomEvent :: Vodozemac.Megolm.InboundGroupSession -> ByteString -> IO (Maybe Event)
decryptRoomEvent session ciphertext =
    Vodozemac.Megolm.InboundGroupSession.decrypt session ciphertext >>= \case
        Just (Aeson.decodeStrict @Aeson.Value -> Just (Aeson.Object plaintext), _) -> do
            Text.putStrLn . Text.decodeUtf8 . LazyByteString.toStrict . Aeson.encodePretty $ plaintext
            case plaintext Aeson.KeyMap.!? "content" of
                Just content | Aeson.Success content' <- Aeson.fromJSON content -> pure . pure $ content'
                _ -> do
                    putStrLn "No content found in plaintext"
                    pure Nothing
        _ -> do
            putStrLn "Message decryption failed"
            pure Nothing

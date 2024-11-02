{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Tuple.Extra (uncurry3)
import Dhall (FromDhall, Generic, ToDhall)
import Dhall qualified
import FRP.Rhine
import FRP.Rhine.Matrix
import Network.Matrix.Client hiding (RoomEvent)
import UploadKeys (uploadKeys)
import Vodozemac.Account qualified
import Vodozemac.Account qualified as Vodozemac (Account)
import Vodozemac.Curve25519PublicKey qualified
import Prelude

data Login
    = Credentials {username :: Text, password :: Text, deviceName :: Maybe Text}
    | SessionToken Text
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data Config = Config
    { baseUrl :: Text
    , room :: Text
    , login :: Login
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data State = State
    { session :: ClientSession
    , account :: Vodozemac.Account
    , userId :: UserID
    , deviceId :: DeviceID
    , identityKey :: ByteString
    }
    deriving stock (Generic)

decryptDeviceEvent :: Text -> MRoomEncryptedPayload -> State -> IO State
decryptDeviceEvent senderKeyBase64 payload state = do
    senderKey <- Vodozemac.Curve25519PublicKey.fromBase64 (Text.encodeUtf8 senderKeyBase64)
    Aeson.Object ciphertext <- pure $ Aeson.toJSON payload
    Vodozemac.Account.createInboundSession state.account senderKey ciphertext >>= \case
        Nothing -> do
            print $ "Failed to decrypt device event: " <> show payload
            pure state
        Just (_session, Aeson.decodeStrict -> Just plaintext) -> do
            putStrLn "Decrypted device event:"
            Text.putStrLn . Text.decodeUtf8 . LazyByteString.toStrict $ Aeson.encodePretty @Aeson.Value plaintext
            pure state
        Just (_session, wtf) -> do
            print $ "Failed to decode device event: " <> show wtf
            pure state

-- Vodozemac.Account.createInboundSession state.account mreSenderKey ciphertext

handleEvents :: ClSF IO MatrixClock State State
handleEvents = proc state -> do
    time <- absoluteS -< ()
    tag <- tagS -< ()
    arrMCl putStrLn -< (show time <> ": " <> show tag)
    case tag of
        ToDeviceMessage ToDeviceEvent{tdeContent = TdMRoomEncrypted MRoomEncrypted{..}}
            | Just ciphertext <- mreCiphertext Map.!? Text.decodeUtf8 state.identityKey ->
                arrMCl (uncurry3 decryptDeviceEvent) -< (mreSenderKey, ciphertext, state)
        _ -> returnA -< state

mainRhine :: State -> MatrixClock -> Rhine IO MatrixClock () ()
mainRhine initialState matrixClock = feedbackRhine (keepLast initialState) $ snd ^>>@ printEventsRh @>>^ ((),)
  where
    printEventsRh = handleEvents @@ matrixClock

main :: IO ()
main = do
    config <- Dhall.inputFile (Dhall.auto @Config) "config.dhall"
    session <-
        case config.login of
            Credentials{..} ->
                Network.Matrix.Client.login
                    LoginCredentials
                        { lUsername = Username username
                        , lLoginSecret = Password password
                        , lInitialDeviceDisplayName = InitialDeviceDisplayName <$> deviceName
                        , lDeviceId = Nothing
                        , lBaseUrl = config.baseUrl
                        }
            SessionToken token -> createSession config.baseUrl (MatrixToken token)
    Right (userId, deviceId) <- getTokenOwner session
    account <- Vodozemac.Account.newAccount
    void $ Vodozemac.Account.generateFallbackKey account
    uploadKeys session account userId deviceId
    identityKey <- Vodozemac.Curve25519PublicKey.toBase64 =<< Vodozemac.Account.curve25519Key account
    joinRoom session "#lounge:synapse.test" >>= print
    flow $ mainRhine State{..} MatrixClock{session, filterId = Nothing, presence = Just Online}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.System (SystemTime(..), utcToSystemTime)
import Data.Tuple.Extra (uncurry3)
import Decrypt
import Dhall (FromDhall, Generic, ToDhall)
import Dhall qualified
import FRP.Rhine
import FRP.Rhine.Matrix
import Network.Matrix.Client (RoomEvent (..))
import Network.Matrix.Client hiding (RoomEvent)
import SendReaction (sendReaction)
import UploadKeys (uploadKeys)
import Vodozemac.Account qualified
import Vodozemac.Account qualified as Vodozemac (Account)
import Vodozemac.Curve25519PublicKey qualified
import Vodozemac.Megolm.InboundGroupSession qualified
import Vodozemac.Megolm.InboundGroupSession qualified as Vodozemac.Megolm (InboundGroupSession)
import Prelude
import Control.Concurrent (threadDelay)
import Redact (sendRedaction)

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
    , megolmSession :: Maybe Vodozemac.Megolm.InboundGroupSession
    }
    deriving stock (Generic)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

handleEvents :: ClSF IO MatrixClock State State
handleEvents = proc state -> do
    time <- absoluteS -< ()
    let systemTime = utcToSystemTime time
    tag <- tagS -< ()
    arrMCl putStrLn -< (show time <> ": " <> show tag)
    case tag of
        ToDeviceMessage ToDeviceEvent{tdeContent = TdMRoomEncrypted MRoomEncrypted{..}}
            | Just ciphertext <- mreCiphertext Map.!? Text.decodeUtf8 state.identityKey -> do
                newSessionKey <- arrMCl (uncurry3 decryptDeviceEvent) -< (state.account, mreSenderKey, ciphertext)
                case newSessionKey of
                    Just key -> do
                        session <- arrMCl Vodozemac.Megolm.InboundGroupSession.new -< key
                        returnA -< state{megolmSession = pure session}
                    Nothing -> returnA -< state
        FRP.Rhine.Matrix.RoomEvent
            ( roomId
                , Network.Matrix.Client.RoomEvent
                    { reType = "m.room.encrypted"
                    , reContent = EventUnknown obj
                    , reEventId
                    }
                )
                | Just session <- state.megolmSession
                , Aeson.Success content <- Aeson.fromJSON @MegolmContent (Aeson.Object obj) -> do
                    event <- arrMCl (uncurry decryptRoomEvent) -< (session, Text.encodeUtf8 content.ciphertext)
                    case event of
                        Just (EventRoomMessage _) -> do
                            Right reactEventId <- arrMCl (uncurry5 sendReaction) -< (state.session, roomId, reEventId, "thumbup-" <> Text.pack (show systemTime.systemSeconds), "👍")
                            arrMCl threadDelay -< 3_000_000
                            arrMCl (uncurry5 sendReaction) -< (state.session, roomId, reEventId, "thumbup-" <> Text.pack (show $ systemTime.systemSeconds + 1), "👎")
                            arrMCl (uncurry4 sendRedaction) -< (state.session, roomId, reactEventId, "thumbdown-" <> Text.pack (show $ systemTime.systemSeconds + 2))
                            returnA -< ()
                        _ -> do
                            arrMCl print -< "Failed to match event: " <> show event
                            returnA -< ()
                    returnA -< state
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
    flow $ mainRhine State{megolmSession = Nothing, ..} MatrixClock{session, filterId = Nothing, presence = Just Online}

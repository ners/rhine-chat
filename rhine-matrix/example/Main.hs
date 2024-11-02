{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Control.Monad (void)
import Data.Text (Text)
import Dhall (FromDhall, Generic, ToDhall)
import Dhall qualified
import FRP.Rhine
import FRP.Rhine.Matrix
import UploadKeys (uploadKeys)
import Network.Matrix.Client
import Vodozemac qualified
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
    }
    deriving stock (Generic)

printEvents :: ClSF IO MatrixClock State State
printEvents = returnA &&& absoluteS &&& tagS >>> arrMCl (\(state, x) -> print x >> pure state)

mainRhine :: State -> MatrixClock -> Rhine IO MatrixClock () ()
mainRhine initialState matrixClock = feedbackRhine (keepLast initialState) $ snd ^>>@ printEventsRh @>>^ ((),)
  where
    printEventsRh = printEvents @@ matrixClock

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
    account <- Vodozemac.newAccount
    void $ Vodozemac.generateFallbackKey account
    uploadKeys session account userId deviceId
    joinRoom session "#lounge:synapse.test" >>= print
    flow $ mainRhine State{..} MatrixClock{session, filterId = Nothing, presence = Just Online}

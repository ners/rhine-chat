{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Data.Text (Text)
import Dhall (FromDhall, Generic, ToDhall)
import Dhall qualified
import FRP.Rhine
import FRP.Rhine.Matrix
import Network.Matrix.Client
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

newtype State = State
    { session :: ClientSession
    }
    deriving stock (Generic)

printEvents :: ClSF IO MatrixClock () ()
printEvents = absoluteS &&& tagS >>> arrMCl print

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
    joinRoom session "#lounge:synapse.test" >>= print
    flow $ printEvents @@ MatrixClock{session, filterId = Nothing, presence = Just Online}

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

newtype State = State
    { session :: ClientSession
    }
    deriving stock (Generic)

printEvents :: ClSF IO MatrixClock () ()
printEvents = absoluteS &&& tagS >>> arrMCl print

main :: IO ()
main = do
    putStrLn "0"
    account <- Vodozemac.newAccount
    putStrLn "1"
    void $ Vodozemac.generateFallbackKey account
    putStrLn "2"
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    putStrLn "3"
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    putStrLn "4"
    void $ Vodozemac.generateFallbackKey account
    putStrLn "5"
    Just (Vodozemac.FallbackKey keyId _) <- Vodozemac.fallbackKey account
    putStrLn "6"
    Vodozemac.keyIdToBase64 keyId >>= putStrLn
    putStrLn "7"
    Vodozemac.freeAccount account

-- config <- Dhall.inputFile (Dhall.auto @Config) "config.dhall"
-- session <-
--    case config.login of
--        Credentials{..} ->
--            Network.Matrix.Client.login
--                LoginCredentials
--                    { lUsername = Username username
--                    , lLoginSecret = Password password
--                    , lInitialDeviceDisplayName = InitialDeviceDisplayName <$> deviceName
--                    , lDeviceId = Nothing
--                    , lBaseUrl = config.baseUrl
--                    }
--        SessionToken token -> createSession config.baseUrl (MatrixToken token)
-- joinRoom session "#lounge:synapse.test" >>= print
-- flow $ printEvents @@ MatrixClock{session, filterId = Nothing, presence = Just Online}

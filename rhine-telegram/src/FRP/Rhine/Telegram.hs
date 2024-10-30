module FRP.Rhine.Telegram where

import Data.Time (getCurrentTime)
import FRP.Rhine
import Servant.Client (runClientM)
import Telegram.Bot.API
    ( GetUpdatesRequest (..)
    , Response (..)
    , Token (..)
    , Update (..)
    , UpdateId (..)
    , defaultTelegramClientEnv
    , getUpdates
    )
import Prelude

newtype TelegramClock = TelegramClock
    { token :: Token
    }

instance (MonadIO m) => Clock m TelegramClock where
    type Time TelegramClock = UTCTime
    type Tag TelegramClock = Update
    initClock :: TelegramClock -> RunningClockInit m (Time TelegramClock) (Tag TelegramClock)
    initClock TelegramClock{..} = liftIO do
        env <- defaultTelegramClientEnv token
        let clock :: Automaton m () (Time TelegramClock, Tag TelegramClock)
            clock = concatS . feedback Nothing $ proc ((), offset) -> do
                let request =
                        GetUpdatesRequest
                            { getUpdatesOffset = offset
                            , getUpdatesLimit = Just 100
                            , getUpdatesTimeout = Just 1
                            , getUpdatesAllowedUpdates = Nothing
                            }
                result <- arrM (liftIO . uncurry runClientM) -< (getUpdates request, env)
                case result of
                    Right Response{..}
                        | responseOk
                        , not (null responseResult) -> do
                            timestamp <- constM (liftIO getCurrentTime) -< ()
                            let updateIdSucc (UpdateId i) = UpdateId $ i + 1
                                newOffset = updateIdSucc . maximum $ updateUpdateId <$> responseResult
                            returnA -< ((timestamp,) <$> responseResult, Just newOffset)
                    _ -> returnA -< ([], offset)
        (clock,) <$> getCurrentTime

instance GetClockProxy TelegramClock

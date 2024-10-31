module FRP.Rhine.Matrix where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Time (getCurrentTime)
import FRP.Rhine
import Network.Matrix.Client
    ( ClientSession
    , FilterID
    , Presence (..)
    , RoomEvent
    , RoomID
    , getTimelines
    , retry
    , srNextBatch
    , sync
    )
import Prelude

data MatrixClock = MatrixClock
    { session :: ClientSession
    , filterId :: Maybe FilterID
    , presence :: Maybe Presence
    }

instance (MonadIO m) => Clock m MatrixClock where
    type Time MatrixClock = UTCTime
    type Tag MatrixClock = (RoomID, RoomEvent)
    initClock :: MatrixClock -> RunningClockInit m (Time MatrixClock) (Tag MatrixClock)
    initClock MatrixClock{..} = liftIO do
        let sync' since = liftIO . retry $ sync session filterId since (Just Online) (Just 10_000)
            clock :: Automaton m () (Time MatrixClock, Tag MatrixClock)
            clock = concatS . feedback Nothing $ proc ((), since) -> do
                syncResult <- arrM sync' -< since
                timestamp <- constM (liftIO getCurrentTime) -< ()
                case syncResult of
                    Right result ->
                        let events =
                                [ (timestamp, (roomId, event))
                                | (roomId, events') <- getTimelines result
                                , event <- NonEmpty.toList events'
                                ]
                         in returnA -< (events, Just result.srNextBatch)
                    Left _ -> returnA -< ([], since)
        (clock,) <$> getCurrentTime

instance GetClockProxy MatrixClock

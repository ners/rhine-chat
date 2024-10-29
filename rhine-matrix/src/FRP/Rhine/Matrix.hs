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
    , srNextBatch
    , sync
    )
import Prelude

data MatrixClock = MatrixClock
    { getSession :: IO ClientSession
    , getFilter :: IO (Maybe FilterID)
    }

instance (MonadIO m) => Clock m MatrixClock where
    type Time MatrixClock = UTCTime
    type Tag MatrixClock = (RoomID, RoomEvent)
    initClock :: MatrixClock -> RunningClockInit m (Time MatrixClock) (Tag MatrixClock)
    initClock MatrixClock{..} = liftIO do
        session <- getSession
        filter' <- getFilter
        let sync' since = liftIO $ sync session filter' since (Just Online) Nothing
            clock :: Automaton m () (Time MatrixClock, Tag MatrixClock)
            clock = concatS . feedback Nothing $ proc ((), since) -> do
                syncResult <- arrM sync' -< since
                timestamp <- constM (liftIO getCurrentTime) -< ()
                case syncResult of
                    Left _ -> returnA -< ([], since)
                    Right result -> do
                        let events =
                                [ (timestamp, (roomId, event))
                                | (roomId, events') <- getTimelines result
                                , event <- NonEmpty.toList events'
                                ]
                            nextSince = pure $ srNextBatch result
                        returnA -< (events, nextSince)
        (clock,) <$> getCurrentTime

instance GetClockProxy MatrixClock

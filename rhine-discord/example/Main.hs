{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This bot connects to a channel and deletes messages from it using exponential decay with a given half-life and decay strategy.
module Main where

import Control.Lens.Operators
import Control.Monad.State.Strict qualified as State
import Data.Either (fromRight)
import Data.Foldable (Foldable (toList))
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.RVar
import Data.Random
import Data.Random.Distribution.Binomial
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as Text
import Data.Tuple (swap)
import Data.Word (Word64)
import Dhall (FromDhall, Generic, Text, ToDhall)
import Dhall qualified
import Discord (DiscordHandler, def, restCall)
import Discord.Internal.Rest.Channel
    ( ChannelRequest (BulkDeleteMessage, GetChannelMessages)
    , MessageTiming (BeforeMessage, LatestMessages)
    )
import Discord.Types
    ( ChannelId
    , DiscordId (..)
    , Event (..)
    , Message (messageChannelId, messageId)
    , MessageId
    , Snowflake (..)
    )
import FRP.Rhine
import FRP.Rhine.Discord
import System.Random
import Prelude

deriving newtype instance Num Snowflake

deriving newtype instance Num (DiscordId a)

data MessageDeletionPolicy
    = DeleteOldest
    | DeleteNewest
    | DeleteRandom
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data Config = Config
    { discordToken :: Text
    , channelId :: Word64
    , initialMessageDeletionPolicy :: MessageDeletionPolicy
    , initialMessageHalfLifeSeconds :: Double
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

data State = State
    { stdGen :: StdGen
    , channelId :: ChannelId
    , messages :: Seq Message
    , messageDeletionPolicy :: MessageDeletionPolicy
    , messageHalfLifeSeconds :: Double
    }
    deriving stock (Generic)

liftRandomGen :: (StdGen -> (b, StdGen)) -> State -> (b, State)
liftRandomGen f st = let (a, g) = f st.stdGen in (a, st{stdGen = g})

instance RandomGen State where
    genWord8 = liftRandomGen genWord8
    genWord16 = liftRandomGen genWord16
    genWord32 = liftRandomGen genWord32
    genWord64 = liftRandomGen genWord64
    genWord32R = liftRandomGen . genWord32R
    genWord64R = liftRandomGen . genWord64R
    genShortByteString = liftRandomGen . genShortByteString
    split st = let (g1, g2) = split st.stdGen in (st{stdGen = g1}, st{stdGen = g2})

type DiscordMillisecond n = HoistClock IO DiscordHandler (Millisecond n)

getAllMessages :: ChannelId -> MessageTiming -> DiscordHandler [Message]
getAllMessages channelId timing = do
    messages <- fromRight [] <$> restCall (GetChannelMessages channelId (100, timing))
    remainingMessages <-
        if null messages
            then pure []
            else getAllMessages channelId . BeforeMessage . messageId $ last messages
    pure $ messages <> remainingMessages

getInitialState :: Config -> DiscordHandler State
getInitialState Config{..} = do
    stdGen <- getStdGen
    messages <- Seq.fromList <$> getAllMessages (fromIntegral channelId) LatestMessages
    pure
        State
            { stdGen
            , channelId = DiscordId . Snowflake $ channelId
            , messages
            , messageDeletionPolicy = initialMessageDeletionPolicy
            , messageHalfLifeSeconds = initialMessageHalfLifeSeconds
            }

handleEventsSF :: ClSF DiscordHandler DiscordEventClock State State
handleEventsSF = proc st -> do
    time <- absoluteS -< ()
    event <- tagS -< ()
    arrMCl (uncurry handleEvent) -< ((time, event), st)

handleEvent :: (Time DiscordEventClock, Tag DiscordEventClock) -> State -> DiscordHandler State
handleEvent (_, event) = State.execStateT do
    st <- State.get
    case event of
        MessageCreate message | message.messageChannelId == st.channelId -> addMessage message
        MessageDelete channelId messageId | channelId == st.channelId -> removeMessages [messageId]
        MessageDeleteBulk channelId messageIds | channelId == st.channelId -> removeMessages messageIds
        _ -> pure ()
  where
    addMessage :: Message -> State.StateT State DiscordHandler ()
    addMessage message = #messages %= (message :<|)
    removeMessages :: [MessageId] -> State.StateT State DiscordHandler ()
    removeMessages messageIds = #messages %= Seq.filter (not . (`elem` messageIds) . (.messageId))

handleLogSF :: ClSF DiscordHandler DiscordLogClock State State
handleLogSF = returnA

simRh :: Rhine DiscordHandler (DiscordMillisecond 1000) State State
simRh = simSF @@ ioClock waitClock

simSF :: (Diff (Time cl) ~ Double) => ClSF DiscordHandler cl State State
simSF = proc model -> do
    dt <- sinceLastS -< ()
    arrMCl (uncurry sim) -< (dt, model)

sim :: Double -> State -> DiscordHandler State
sim dt = State.execStateT do
    State{..} <- State.get
    let k = messageHalfLifeSeconds / dt
        n = length messages
        p = 2 ** (-1 / k)
        q = 1 - p
    dn <- sampleState $ Binomial n q
    (decayedMessages, aliveMessages) <-
        case messageDeletionPolicy of
            DeleteOldest -> pure . swap $ Seq.splitAt dn messages
            DeleteNewest -> pure $ Seq.splitAt (n - dn) messages
            DeleteRandom -> do
                indices <- sampleStateRVar $ shuffleNofM dn n [0 .. n - 1]
                let indexed :: Seq a -> Seq (Int, a)
                    indexed = Seq.zip (Seq.fromList [0 ..])
                    unindexed :: Seq (Int, a) -> Seq a
                    unindexed = fmap snd
                    (decayed, alive) = Seq.partition (flip elem indices . fst) $ indexed messages
                pure (unindexed decayed, unindexed alive)
    #messages .= aliveMessages
    let decayedMessageIds = toList $ (.messageId) <$> decayedMessages
    void . State.lift . restCall $ BulkDeleteMessage (channelId, decayedMessageIds)

main :: IO ()
main = do
    config <- Dhall.inputFile (Dhall.auto @Config) "config.dhall"
    flowDiscord config.discordToken def (getInitialState config) handleEventsSF handleLogSF simRh >>= Text.putStrLn

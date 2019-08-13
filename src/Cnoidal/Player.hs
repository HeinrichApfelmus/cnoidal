{-----------------------------------------------------------------------------
    Player
    At each measure, the player asks which notes s/he should play.
    He then sends them to the MIDI device
------------------------------------------------------------------------------}
module Cnoidal.Player (
    -- * Synopsis
    -- | Play MIDI in real time.
    --
    -- A 'Player' plays musical snippet, represented by 'Media', in repeat.
    --
    -- An 'Ensemble' groups 'Player's so that they follow the same beat.
    
    -- * MIDI
    Channel, openMidi, closeMidi,
    
    -- * Player
    Player, newPlayer, play,
    
    -- * Ensemble
    Ensemble, newEnsemble, together, dissolve,
    setTempoBpm,
    ) where

import           Control.Applicative
import           Control.Concurrent   (threadDelay, ThreadId, killThread, forkIO)
import           Control.Exception
import           Control.Monad        (void, join)
import           Data.IORef
import qualified Data.List
import           Data.Maybe           (maybe)
import           Data.Ord             (comparing)
import           Data.Ratio

import Sound.PortMidi as Midi

import Cnoidal.Media
import Cnoidal.Music

{-----------------------------------------------------------------------------
    MIDI
------------------------------------------------------------------------------}
-- | A MIDI channel
type Channel = Int

-- | Open the default output MIDI device.
openMidi :: IO Midi.PMStream
openMidi = do
    initialize
    Just dOut <- getDefaultOutputDeviceID
    Right s <- openOutput dOut 1
    return s

-- | Close all MIDI devices.
closeMidi = terminate

{-----------------------------------------------------------------------------
    Player
------------------------------------------------------------------------------}
-- | A 'Player' repeats a piece of music, which can be changed on the fly.
data Player = Player
    { pChannel :: Int
    , pNotes   :: IORef (Musicbox Note)
    }

-- | A collection of musical notes, grouped by measures.
-- Each element of the outer list is one measure.
-- The starting and ending times of each interval are relative to this measure.
type Musicbox a = [[(Interval, a)]]

fromMediaCycle :: Media a -> Musicbox a
fromMediaCycle media = mycycle $ map (measure . fromIntegral) [0..len-1]
    where
    measure t = [ ((subtract t t1, (subtract t) `fmap` s2), a)
                | ((t1,s2),a) <- toIntervals media, t <= t1, t1 < t+1]
    len   = forwardToInteger $ maybe 1 id (duration media)
    mycycle xs = if null xs then [] else Data.List.cycle xs

forwardToInteger :: Time -> Integer
forwardToInteger x = if r == 0 then q else q + 1
    where (q,r) = numerator x `divMod` denominator x

-- | Retrieve current measure to be played.
getMeasure :: Player -> IO [(Interval, Note)]
getMeasure = fmap (\xs -> if null xs then [] else head xs) . readIORef . pNotes

-- | Advance the measure to be played and retrieve the current one.
nextMeasure :: Player -> IO ()
nextMeasure p = do
    music <- readIORef (pNotes p)
    case music of
        []   -> return ()
        x:xs -> writeIORef (pNotes p) xs

-- | Create a new player that participates in the ensemble.
-- The input argument denotes the MIDI channel on which the player outputs notes.
newPlayer :: Channel -> IO Player
newPlayer channel = do
    notes <- newIORef []
    return $ Player { pChannel = channel, pNotes = notes }


-- | Set the piece for a 'Player' to play, beginning at the next full measure.
play :: IsNote a => Player -> Media a -> IO ()
play p = writeIORef (pNotes p) . fromMediaCycle . fmap toNote


{-----------------------------------------------------------------------------
    Ensemble
------------------------------------------------------------------------------}
data Ensemble = Ensemble
    { eThreadId :: ThreadId
    , ePlayers  :: IORef [Player]
    , eBpm      :: IORef Rational
    }

-- | Create a new ensemble for players to join.
newEnsemble :: Midi.PMStream -> IO Ensemble
newEnsemble out = do
        players <- newIORef []
        eBpm    <- newIORef 120
        thread  <- forkIO $
            every (timePerMeasure 1 `fmap` readIORef eBpm) $ \millisecs -> do
                bpm <- readIORef eBpm
                playNotes millisecs bpm =<< nextMeasures =<< readIORef players
                return ()
        return $ Ensemble { eThreadId = thread, ePlayers = players, eBpm = eBpm }
    where
    timePerMeasure t bpm = round $ t * (4 / bpm) * 60 * 1000
    playNotes      t bpm = -- see Note [Midi.writeEvents]
        Midi.writeEvents out . Data.List.sortBy (comparing timestamp) . concatMap mkEvent
        where
        mkEvent (t1, t2, ((pitch, vel), channel)) =
            [ PMEvent { message = noteOn , timestamp = fromIntegral $ t + timePerMeasure t1 bpm }
            , PMEvent { message = noteOff, timestamp = fromIntegral $ t + timePerMeasure t2 bpm - 1 }
            ]
            where
            noteOn  = encodeMsg $ PMMsg { status = 0x90 + fromIntegral channel - 1, data1 = fromIntegral pitch, data2 = fromIntegral vel }
            noteOff = encodeMsg $ PMMsg { status = 0x80 + fromIntegral channel - 1, data1 = fromIntegral pitch, data2 =  0 }

{- Note [Midi.writeEvent]

The `Midi.writeEvents` function expects the timestampes to be in increasing order.
That is why we need to sort our events by timestamp before sending them.
If we do not do that, MIDI messages whose timestamp is too early will be dropped.

-}

{- FIXME
Notes that have a duration that goes beyond the current measure currently
break everything.
The `Midi.writeEvents` functions expects messages to be written in
increasing time stamps. There is no way to "sneak" a midi message in before.
But this may happen when the measure changes.
In particular, it may happen that the "note off" event appears after a new "note on" event.s
-}

-- | Get the measure that the players are currently playing.
nextMeasures :: [Player] -> IO [(Time, Time, (Note, Channel))]
nextMeasures = fmap concat . mapM measure
    where
    measure p = do
        xs <- getMeasure p
        nextMeasure p
        return [(t, maybe (t+3) id s, (a, pChannel p)) | ((t,s),a) <- xs]

-- | Set the tempo (in beats per minute)
setTempoBpm :: Ensemble -> Rational -> IO ()
setTempoBpm e = writeIORef (eBpm e)

-- | Set the current players in an ensemble.
together :: Ensemble -> [Player] -> IO ()
together e players = writeIORef (ePlayers e) players

-- | Stop an ensemble from playing
dissolve :: Ensemble -> IO ()
dissolve = killThread . eThreadId

{-----------------------------------------------------------------------------
    Scheduling
------------------------------------------------------------------------------}
-- NOTE:
--   Midi time (Midi.time)   is milliseconds (ms)
--   GHC  time (threadDelay) is microseconds (µs)

type Milliseconds = Int

-- | Perform an action in regular intervals.
-- The interval length is obtained by performing an 'IO' action.
--
-- The action is started 10 milliseconds before the interval ends.
-- Based on the Midi timer.
every :: IO Milliseconds -> (Milliseconds -> IO ()) -> IO ()
every getInterval action = do
        starttime <- (+ maxdelay) . fromIntegral <$> Midi.time
        milliseconds <- getInterval
        action starttime
        go (starttime + milliseconds)
    where
    maxdelay = 10 -- milliseconds that the action may use up
    go time = do
        now <- Midi.time
        threadDelay $ 1000 * (time - fromIntegral now - maxdelay)
        milliseconds <- getInterval
        action time
        go (time + milliseconds)


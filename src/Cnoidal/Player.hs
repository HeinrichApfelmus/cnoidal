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

import Control.Applicative
import Control.Concurrent   (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception
import Control.Monad        (void, join)
import Data.IORef
import Data.List            (sortBy)
import Data.Maybe           (maybe)
import Data.Ord             (comparing)
import Data.Ratio

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
data Player = Player
    { pChannel :: Int
    , pNotes   :: IORef (IO [(Time, Time, Pitch)])
    }

-- | Create a new player that participates in the ensemble.
newPlayer :: Channel -> IO Player
newPlayer channel = do
    notes <- newIORef (return [])
    return $ Player { pChannel = channel, pNotes = notes }

-- | Set the pattern for a player to play.
play      :: Player -> Media Pitch -> IO ()
play p pat = writeIORef (pNotes p) =<< scheduleChords pat

-- | Schedule 'Media' to be played by a player.
--
-- The media is cycled at the smallest integer such that notes are still played
--
-- The IO action returns the next full measure of notes to be played.
-- The starting times begin an @0@ and are relative to the start of the measure.
scheduleChords :: Media Pitch -> IO (IO [(Time, Time, Pitch)])
scheduleChords media = do
    let len = measures media
    if len == 0
        then return (return [])
        else do
            count <- newIORef 0
            return $ do
                c <- readIORef count
                writeIORef count $! (c + 1) `mod` len
                let t = fromIntegral c
                let notes = [(t1 - t, maybe 1 (\t2 -> t2-t1) s2, a) |
                        ((t1,s2),a) <- toIntervals media, t <= t1, t1 < t+1]
                return notes

-- Count the number of measures in the given media.
measures :: Ord a => Media a -> Integer
measures media = case toIntervals media of 
    [] -> 0
    xs -> forwardToInteger . maximum . map (start . fst) $ xs

forwardToInteger :: Time -> Integer
forwardToInteger x = q + 1
    where (q,r) = numerator x `divMod` denominator x

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
        id      <- forkIO $
            every (timePerMeasure 1 `fmap` readIORef eBpm) $ \millisecs -> do
                bpm <- readIORef eBpm
                playNotes millisecs bpm =<< getMeasure =<< readIORef players
                return ()
        return $ Ensemble { eThreadId = id, ePlayers = players, eBpm = eBpm }
    where
    timePerMeasure t bpm = round $ t * (4 / bpm) * 60 * 1000
    playNotes      t bpm =
        Midi.writeEvents out . sortBy (comparing timestamp) . concatMap mkEvent
        where
        mkEvent (t1, t2, (pitch, channel)) =
            [ PMEvent { message = noteOn  , timestamp = fromIntegral $ t + timePerMeasure t1 bpm }
            , PMEvent { message = noteOff, timestamp = fromIntegral $ t + timePerMeasure (t1+t2) bpm - 1 }
            ]
            where
            noteOn  = encodeMsg $ PMMsg { status = 0x90 + fromIntegral channel , data1 = fromIntegral pitch, data2 = 80 }
            noteOff = encodeMsg $ PMMsg { status = 0x80 + fromIntegral channel , data1 = fromIntegral pitch, data2 =  0 }

-- | Get the measure that the player is currently playing.
getMeasure :: [Player] -> IO [(Time, Time, (Pitch, Channel))]
getMeasure = fmap concat . mapM measure
    where
    measure p = do
        xs <- Control.Monad.join $ readIORef $ pNotes p
        return [(t1, t2, (a, pChannel p)) | (t1,t2,a) <- xs]

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


module Cnoidal.Music (
    -- * Synopsis
    -- | Data structures and functions for representing music.
    
    -- * Rhythm
    Beat,
    quarter, quaver, beat, tim,
    campfire,
    bd, sn, rim, hh, chh, ohh, crash,
    
    -- * Melody
    Pitch, dore, absolute,
    
    -- * Harmony
    Chord, chords, chord,
    ) where

import           Control.Monad

import qualified Data.Char      as Char
import qualified Data.Map       as M
import           Data.Map               (Map, (!))
import           Data.Monoid            ((<>))

import           Cnoidal.Media  as C

{-----------------------------------------------------------------------------
    Interesting patterns
------------------------------------------------------------------------------}
test :: Media String
test = slow quarter $ fromList $ words "bd bd sd sn"

example1 :: Media Chord
example1 = (fromList $ chords "am F C G") <*
    (bind (fromList $ replicate 4 $ campfire) id)

{-----------------------------------------------------------------------------
    Rhythm
------------------------------------------------------------------------------}
-- | A rhythm is about the timing of notes.
--   The values are unimportant, so we use the unit type '()'.
type Beat = Media ()

quarter, quaver :: Time
quarter = 1/4
quaver  = 1/8 

-- | Map rhythm words into their beats.
tim :: Map String Beat
tim = (hasten 16 . beat) <$> associate
    "schwein eisbaer seerobbe schmetterling ringelnatter giraffe"
    "x... x.x. x.xx xx.x xxxx .xxx"
    where
    associate xs ys = M.fromList $ zip (words xs) (words ys)

-- | Create a beat from a string.
--
-- Example: @beat "x,,, x,,,"@
--
-- Whitespace is removed. Each symbol has unit length.
-- The symbol @x@ corresponds to a beat. The other symbols correspond to silence.
beat :: String -> Beat
beat = fmap (const ()) . C.filter (== 'x')
     . fromList . Prelude.filter (not . Char.isSpace)

-- | Example rhythm.
campfire = mconcat $ map (tim !) $ words "seerobbe giraffe seerobbe giraffe"

fromQuavers = hasten 4 . fromList

-- | Commonly used percussion instruments, as in the General MIDI standard.
--
-- See https://en.wikipedia.org/wiki/General_MIDI#Percussion
--
-- > bd    = bass drum
-- > sn    = snare drum
-- > rim   = rimshot
-- > hh    = hi-hat (pedal)
-- > chh   = hi-hat (closed)
-- > ohh   = hi-hat (open)
-- > crash = cymbal
bd :: Pitch
rim, sn, hh, chh, ohh, crash :: Pitch
[bd, rim, sn, hh, chh, ohh, crash] = [36,37,38, 44,42,43, 49]

{-----------------------------------------------------------------------------
    Melody
------------------------------------------------------------------------------}
-- | A pitch is specified by a number of semitones.
--   Middle C corresponds to @60@, as in the General sMIDI standard.
type Pitch   = Int

-- | Movable do notation.
dore :: Map String Pitch
dore = associate "do re mi fa so la ti" [0,2,4,5,7,9, 11]
    <> associate "di ri    fi si li   " [1,3,  6,8,10]
    <> associate "   ra me    se le te" [  1,3,  6,8, 10]
    where
    associate xs ys = M.fromList $ zip (words xs) ys

-- | Absolute pitch names.
absolute :: Char -> Pitch
absolute = f . Char.toLower
    where
    f 'c' = 0; f 'a' = 9; f 'b' = 11;
    f 'd' = 2; f 'e' = 4; f 'f' = 5;  f 'g' = 7;

{-
instance IsString (Media (Maybe Pitch)) where
    fromString = id
        . fmap (fmap (+60)) . slow quarter . fromList
        . map (`M.lookup` dore) . words
-}

{-----------------------------------------------------------------------------
    Chords
------------------------------------------------------------------------------}
-- | A 'Chord' is a collection of pitches, to be played at the same time.
type Chord   = [Pitch]

-- | Apply 'chord' to a list of chord notations.
chords :: String -> [Chord]
chords = map chord . words

-- | Map Jazz chord notation into list of absolute pitches.
chord :: String -> Chord
chord (c:cs) = map (60+) $ notes $ case cs of
        ""  -> "do mi so"
        "m" -> "do me so"
    where
    notes = map (\x -> absolute c + dore ! x) . words


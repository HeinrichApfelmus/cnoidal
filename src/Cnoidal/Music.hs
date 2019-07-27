module Cnoidal.Music (
    -- * Synopsis
    -- | Data structures and functions for representing music.
    
    -- * Rhythm
    Beat,
    quarter, quaver, beat, tim,
    campfire,
    bd, sn, rim, hh, chh, ohh, crash,
    
    -- * Melody
    Pitch, pitch, pitches, dore,
    
    -- * Bass
    root, roots,
    
    -- * Harmony
    Chord, chord, chords, voicelead,
    ) where

import           Control.Monad

import qualified Data.Char      as Char
import qualified Data.List      as Data
import qualified Data.Map       as M
import           Data.Map               (Map, (!))
import           Data.Monoid            ((<>))
import qualified Data.Ord       as Data

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
-- | A pitch is specified by a number of semitones above a given reference note.
-- We follow the conventions of the General MIDI standard.
type Pitch = Int

-- | The middle C. It equals @60@ in the General MIDI standard.
middleC, c4 :: Pitch
middleC = 60
c4      = middleC

-- | Movable do notation.
dore :: Map String Pitch
dore = associate "do re mi fa so la ti" [0,2,4,5,7,9, 11]
    <> associate "di ri    fi si li   " [1,3,  6,8,10]
    <> associate "   ra me    se le te" [  1,3,  6,8, 10]

associate :: String -> [a] -> Map String a
associate xs ys = M.fromList $ zip (words xs) ys

-- | Map pitch names to 'Pitch'.
pitch :: String -> Pitch
pitch = fst . pitchP

-- | Apply 'pitch' to a list of note names that are separated by whitespace.
pitches :: String -> [Pitch]
pitches = map pitch . words

-- | Parse pitch name.
pitchP :: String -> (Pitch, String)
pitchP = name middleC
    where
    name x []        = (x, "")
    name x (c:cs)    = case M.lookup [Char.toLower c] names of
        Nothing -> sharpen x (c:cs)
        Just d  -> sharpen (x+d) cs
    sharpen x []     = (x, "")
    sharpen x (c:cs) = case M.lookup [c] sharps of
        Nothing -> octave x (c:cs)
        Just d  -> octave (x+d) cs
    octave  x []     = (x, "")
    octave  x (c:cs) = case M.lookup [c] octaves of
        Nothing -> (x, c:cs)
        Just d  -> (x+d, cs)

    octaves = associate "0 1 2 3 4 5 6 7 8" [ 12*(y-4) | y <- [0..8] ]
    sharps  = associate "# b" [1,-1]
    names   = associate "c d e f g a b" [0,2,4,5,7,9,11]

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

-- | Map Jazz chord notation into list of absolute pitches.
chord :: String -> Chord
chord s = case pitchP s of
      (x,cs) -> map (x+) $ notes $ case cs of
          ""  -> "do mi so"
          "m" -> "do me so"
    where
    notes = map (dore !) . words

-- | Apply 'chord' to a list of chord notations that are separated by whitespace.
chords :: String -> [Chord]
chords = map chord . words

-- | Return root note for Jazz chord notation.
root :: String -> Pitch
root s = case pitchP s of (x,_) -> x

-- | Apply 'root' to a list of chord notations that are separated by whitespace.
roots :: String -> [Pitch]
roots = map root . words


-- | Use voice lead
voicelead :: [Chord] -> [Chord]
voicelead [] = []
voicelead xs = Data.scanl1 lead xs
    where
    lead x y     = Data.minimumBy (Data.comparing $ distance x) $ inversions y
    inversions x = take 5 (iterate invertUp x) ++ take 5 (iterate invertDown x)
    distance x y = sum $ map abs $ zipWith (subtract) x y

-- | Invert chord upwards one step.
invertUp :: Chord -> Chord
invertUp []     = []
invertUp (x:xs) = xs ++ [x + 12]

-- | Invert chord downwards one step.
invertDown :: Chord -> Chord
invertDown [] = []
invertDown xs = (last xs - 12) : init xs


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Cnoidal.Music (
    -- * Synopsis
    -- | Data structures and functions for representing music.
    
    -- * Rhythm
    Velocity, ppp, pp, piano, mp, fff, ff, forte, mf,
    Beat,
    quarter, quaver, beat, tim,
    campfire,
    bd, sn, rim, hh, chh, ohh, crash,
    
    -- * Melody
    Pitch, middleC, c4, octave, pitch, pitches, dore,
    Scale, at, major, minor, majorPenta, minorPenta,
    Note, IsNote(..), silence, with,
    
    -- * Bass
    root, roots,
    
    -- * Harmony
    Chord, chord, chords, voicelead,
    ) where

import           Control.Monad
import           Control.Applicative    (empty)

import qualified Data.Char      as Char
import qualified Data.List      as Data
import qualified Data.Map       as M
import           Data.Map               (Map, (!))
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
-- | MIDI velocity. Ranges from `0` (softest) to `127` (loudest).
type Velocity = Int

-- | Common interpretation of musical dynamics in terms of MIDI velocities,
-- following MuseScore 3.0.
-- See also https://en.wikipedia.org/wiki/Dynamics_%28music%29
--
-- > ppp = 16   pp = 33   piano = 49  mp = 64
-- > fff = 126  ff = 112  forte = 96  mf = 80
--
mf :: Velocity
ppp, pp, piano, mp, fff, ff, forte :: Velocity
[mf, ppp,pp,piano,mp, fff,ff,forte] = [80,16,33,49,64,126,112,96]

-- | A rhythm, or 'Beat', denotes the timing of sound events, e.g. strokes on a drum.
-- However, the 'Velocity' of the strokes is also important,
-- giving rise to accented beats.
type Beat = Media Velocity

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
-- Example: @beat "x,,X ,,,,"@
--
-- Whitespace is removed. Each symbol has unit length. The symbols are
--
-- > 'x' = mezzoforte
-- > 'X' = forte
--
-- All other symbols correspond to silence.
beat :: String -> Beat
beat = C.filterJust . fromList . map f . Prelude.filter (not . Char.isSpace)
     where
     f 'x' = Just mf
     f 'X' = Just forte
     f _   = Nothing

-- | Example rhythm.
campfire = mconcat $ map (tim !) $ words "seerobbe giraffe seerobbe giraffe"

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

-- | An 'octve' has @12@ semitones.
octave :: Pitch
octave = 12

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

-- | A 'Scale' represents a musical scale as a list of semitones relative
-- to the root note.
type Scale = [Pitch]

major, minor, majorPenta, minorPenta :: Scale
major = [0,2,4,5,7,9,11]
minor = [0,2,3,5,7,8,10]
majorPenta = [0,2,4,7,9]
minorPenta = [0,3,5,7,10]

-- | Retrieve the 'Pitch' of a scale note.
-- The scale repeats at every octave.
at :: Scale -> Int -> Pitch
at scale k = octave*q + (scale !! r)
    where (q,r) = (k-1) `divMod` length scale


-- | A 'Note' informs the sound that an instrument makes when struck.
-- Essentially, it combines pitch and velocity.
--
-- In colloquial usage, the term `note` often includes the duration of the sound as
-- well, e.g. ``a quaver note``, but sometimes it does not,
-- e.g. ``black note on the keyboard``.
-- Here, we explicitly do /not/ include the duration.
type Note = (Pitch, Velocity)

-- | Convenience class for specifying 'Note' more readily.
class IsNote a where
    toNote :: a -> Note

instance IsNote Note   where toNote   = id
instance IsNote Pitch  where toNote p = (p, mf)
instance IsNote String where toNote   = toNote . pitch

-- | Synonym for 'empty' with a more specific type.
silence :: Media Note
silence = mempty

-- | Merge a 'Pitch' and a 'Velocity' into a 'Note'.
with :: Pitch -> Velocity -> Note
with = curry id

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


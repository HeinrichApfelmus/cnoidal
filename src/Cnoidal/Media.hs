{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Cnoidal.Media  (
    -- * Synopsis
    -- | Temporal media for representing music.
    
    -- * Time and Intervals
    Time,
    Interval, start, end, earlier, later, intersection, intersect, disjoint,
    
    -- * Temporal Media
    Media, duration, toIntervals, fromInterval, fromList,
    filter, slow, hasten,
    shift, staircase,
    polyphony, bind,
    envelope,
    trim, cut,
    ) where

import           Prelude           hiding (filter)
import qualified Data.List         as List
import qualified Data.Map          as Map
import           Data.Map               (Map,(!))
import           Data.Maybe
import           Data.Monoid
import           Data.Ord             (comparing)
import           Data.Ratio
import           Data.String

import Control.Applicative
import Control.Monad

import Data.IORef

{-----------------------------------------------------------------------------
    Time
------------------------------------------------------------------------------}
-- | @Time@ is an exact rational number.
-- For music, we use the convention that a full measure has length @1@.
type Time = Rational

{-----------------------------------------------------------------------------
    Intervals
------------------------------------------------------------------------------}
-- | Time intervals start at a time @>=0@, but may be infinite.
type Interval = (Time, Maybe Time)

-- | Starting time of an interval.
start :: Interval -> Time
start (t1,_) = t1

-- | Ending time of an interval. The interval is half-infinite if this is 'Nothing'.
end :: Interval -> Maybe Time
end (_,t2) = t2

-- | Test whether the first interval ends before or when the second begins.
earlier :: Interval -> Interval -> Bool
earlier (_, Just t2) (s1,_) = t2 <= s1
earlier _             _     = False

-- | Test whether the first interval begins after or when the second ends.
later = flip earlier

-- | Intersection of two intervals.
intersection :: Interval -> Interval -> Maybe Interval
intersection (t1,t2) (s1,s2) = legalize (max t1 s1, minMaybe t2 s2)
    where
    legalize (t1,Just t2) | t1 >= t2 = Nothing
    legalize x                       = Just x

-- | Test whether two intervals intersect.
-- Corner case: Two interval do not intersect if they touch at the endpoints.
intersect :: Interval -> Interval -> Bool
intersect a b = isJust $ intersection a b

-- | Test whether two intervals are disjoint.
disjoint t s = not $ intersect t s

{-----------------------------------------------------------------------------
    Media
------------------------------------------------------------------------------}
-- | 'Media' denotes a collection of time intervals with associated values.
data Media a = Media
    { duration    :: Maybe Time         -- ^ Total duration.
    , toIntervals :: [(Interval, a)]    -- ^ List of intervals and values.
    }
    -- Invariant: All starting times are >= 0.
    -- Invariant: Starting times of intervals appear in order
    deriving (Eq, Ord, Show)

instance Functor Media where
    fmap f (Media d xs) = Media d $ map (\(a,x) -> (a,f x)) xs

-- | Sequence of intervals of unit length whose values are from the given list
fromList :: [a] -> Media a
fromList xs = Media (Just $ fromIntegral $ length xs)
    [((k,Just $ k+1),x) | (k,x) <- zip [0..] xs]

-- | Create 'Media' from a single 'Interval' and value.
fromInterval :: (Interval, a) -> Media a
fromInterval (t,x) = Media (end t) [(t,x)]

-- | Smallest interval that contains the whole Media
envelope :: Media a -> Interval
envelope (Media _ xs) = (minimum (map (fst.fst) xs), maximum (map (snd.fst) xs))
    where
    maximum = foldr maxMaybe (Just 0)

-- | Keep only those intervals where the value satisfies a predicate
filter :: (a -> Bool) -> Media a -> Media a
filter p (Media d xs) = Media d $ List.filter (p . snd) xs

-- | Transform the interval times. 
-- Do *not* export this function, as it can break the invariant.
mapTimes_ :: (Time -> Time) -> [(Interval, a)] -> [(Interval, a)]
mapTimes_ f xs = [ ((f t1, fmap f t2), x) | ((t1,t2),x) <- xs] 

-- | Multiply all times by a common factor, making the intervals longer.
slow :: Rational -> Media a -> Media a
slow s (Media d xs) = Media d $ mapTimes_ (*s) xs

-- | Divide all times by a common factor, making the intervals shorter.
hasten :: Rational -> Media a -> Media a
hasten s (Media d xs) = Media (fmap (/s) d) $ mapTimes_ (/s) xs

-- | Shift all times by a time difference.
-- The assigned duration will *not* be shifted.
--
-- Note: The intervals will be cut off if the starting time is negative.
shift :: Time -> Media a -> Media a
shift dt = trim (0, Nothing) . shift_ dt

shift_ dt (Media d xs) = Media d $ mapTimes_ (+dt) xs

-- | Repeatedly shift a sequence of media and stack the in parallel.
staircase :: Time -> [Media a] -> Media a
staircase dt xs = asum [(shift (fromIntegral n) x) | (x,n) <- zip xs [0..]]
    where asum = foldr (<|>) empty

-- | Trim a Media to a time interval.
trim :: Interval -> Media a -> Media a
trim dt = fst . cut dt

-- | Cut a 'Media' into two pieces according to a time interval.
--
-- The first 'Media' will have duration equal to the ending time of the first argument
-- The second 'Media' will be shifted to start at 0.
cut :: Interval -> Media a -> (Media a, Media a)
cut dt (Media d xs) = (Media (end dt) ys1, Media duration2 ys2)
    where
    duration2 = case end dt of
        Nothing -> Just 0
        Just dt -> fmap (\d -> max 0 (d-dt)) d
    ys2 = case snd dt of
        Nothing -> []
        Just t2 -> [(dy, x) | (dx, x) <- xs, Just dy <- [intersection (t2,Nothing) dx]]
    ys1 = [(dy, x) | (dx, x) <- xs1, Just dy <- [intersection dt dx]]
    xs1 = takeWhile (not . earlier dt . fst) xs

-- TODO: Implement function spanInterval that works with an interval
-- This can then be used with `unfold` !

-- | Replace each value with a sequence of intervals by itself.
-- The duration of the result is the duration of the first argument.
-- 
-- Very similar to the monadic bind '(>>=)', but does not satisfy the monad laws,
-- and is not compatible with the 'Applicative' instance.
bind :: Media a -> (a -> Media b) -> Media b
bind (Media d xs) g =  Media d $ concat [ y | Media _ y <- ys ]
    where
    ys = [ trim dt $ shift_ (start dt) (g a) | (dt,a) <- xs ]

-- | Turn lists of values into multiple intervals.
polyphony :: Media [a] -> Media a
polyphony (Media d xs) = Media d [(t,a) | (t,as) <- xs, a <- as]

-- | Intersect intervals pairwise and apply function.
apply :: [(Interval, a -> b)] -> [(Interval, a)] -> [(Interval, b)]
apply []          _  = []
apply ((tf,f):fs) xs = [(x,y) | (Just x,y) <- results] ++ apply fs xs'
    where
    results = takeWhile (isJust . fst) [ (intersection tf tx, f x) | (tx, x) <- xs']
    xs'     = dropWhile (later tf . fst) xs

instance Applicative Media where
    pure x = Media Nothing [((0,Nothing), x)]
    (Media df fs) <*> (Media dx xs) = Media (minMaybe df dx) $ apply fs xs

-- | Parallel composition.
instance Alternative Media where
    empty = Media (Just 0) []
    (Media dx xs) <|> (Media dy ys)
        = Media (maxMaybe dx dy) $ List.sortBy (comparing (fst.fst)) $ xs ++ ys

-- | Sequential composition.
instance Monoid (Media a) where
    mempty = Media (Just 0) []
    mappend x@(Media dx xs) (Media dy ys) = case dx of
        Nothing -> x
        Just dx -> Media (fmap (+dx) dy) (xs ++ mapTimes_ (+dx) ys)

{-----------------------------------------------------------------------------
  Helper functions
------------------------------------------------------------------------------}
-- | Replace each element in the list by a unique natural number,
-- starting at @1@ and counting upwards.
uniques :: Ord a => [a] -> [Integer]
uniques = go 1 Map.empty
    where
    go _  _  []     = []
    go !n !m (x:xs) = case Map.lookup x m of
        Nothing -> n : go (n+1) (Map.insert x n m) xs
        Just k  -> k : go n m xs

-- | Take the maximum of two maybes. 'Nothing' represents infinity.
maxMaybe :: Maybe Time -> Maybe Time -> Maybe Time
maxMaybe (Just a) (Just b) = Just (max a b)
maxMaybe _        _        = Nothing

-- | Take the minimum of two maybes. 'Nothing' represents infinity.
minMaybe :: Maybe Time -> Maybe Time -> Maybe Time
minMaybe = unionWith min

-- | Combine two maybe values.
unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith f Nothing  y        = y
unionWith f x        Nothing  = x
unionWith f (Just x) (Just y) = Just (f x y)


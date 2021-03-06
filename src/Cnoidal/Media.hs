{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Cnoidal.Media  (
    -- * Synopsis
    -- | Temporal media for representing music.
    
    -- * Time and Intervals
    Time,
    Interval, start, end, earlier, later, intersection, intersect, disjoint,

    -- * Temporal Media
    Media, duration, toIntervals, isEmpty,
    fromInterval, fromIntervals, fromList, list, cycle,
    filter, filterJust, flow,
    slow, hasten, sustain, shift,
    polyphony, bind, adorn,
    envelope,
    splitTime, trim, cut,

    module Control.Applicative,
    module Data.Semigroup
    ) where

import           Prelude           hiding (filter, cycle)
import qualified Data.List         as List
import           Data.Maybe
import           Data.Semigroup
import           Data.Ord             (comparing)
import           Data.Ratio
import           Data.String

import Control.Applicative
import Control.Monad

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
    --
    -- FIXME: Be more pedantic about invariants!
    deriving (Eq, Ord, Show)

-- | Test whether a 'Media' is empty, i.e. has duration @0@ and contains no intervals.
isEmpty :: Media a -> Bool
isEmpty (Media (Just 0) []) = True
isEmpty _                   = False

instance Functor Media where
    fmap f (Media d xs) = Media d $ map (\(a,x) -> (a,f x)) xs

-- | Sequence of intervals of unit length whose values are from the given list
fromList :: [a] -> Media a
fromList xs = Media (Just $ fromIntegral $ length xs)
    [((k,Just $ k+1),x) | (k,x) <- zip [0..] xs]

-- | Shorter synonym for 'fromList'
list :: [a] -> Media a
list = fromList

-- | Create 'Media' from a single 'Interval' and value.
fromInterval :: (Interval, a) -> Media a
fromInterval (t,x) = Media (end t) [(t,x)]

-- | Create 'Media' from a duration and a list of 'Interval's with values.
--
-- Note: This function is provided to allow direct manipulation of 'Interval's
-- in temporal media. In practice, chances are that the desired manipulation
-- can already be expressed by combining other operations provided in this module.
fromIntervals :: Maybe Time -> [(Interval, a)] -> Media a
fromIntervals d = Media d . List.sortOn (start . fst)

-- | Smallest interval that contains the whole Media
envelope :: Media a -> Interval
envelope (Media _ xs) = (minimum (map (fst.fst) xs), maximum (map (snd.fst) xs))
    where
    maximum = foldr maxMaybe (Just 0)

-- | Keep only those intervals where the value satisfies a predicate
filter :: (a -> Bool) -> Media a -> Media a
filter p (Media d xs) = Media d $ List.filter (p . snd) xs

-- | Keep only those intervals whose value is `Just`.
filterJust :: Media (Maybe a) -> Media a
filterJust (Media d xs) = Media d $ [(t,x) | (t,Just x) <- xs]

-- | Align a list of values to a collection of intervals
-- in order of increasing starting time.
flow :: [a] -> Media some -> Media a
flow = flowWith const

-- | Align a list of values to a collection of intervals
-- in order of increasing starting time.
--
-- FIXME: I think there is a 'Traversable' instance hidden in this.
flowWith :: (a -> b -> c) -> [a] -> Media b -> Media c
flowWith f xs (Media d ys) = Media d $ zipWith (\x (t,y) -> (t,f x y)) xs ys

-- | Transform the interval times. 
-- Do *not* export this function, as it can break the invariant.
mapTimes_ :: (Time -> Time) -> [(Interval, a)] -> [(Interval, a)]
mapTimes_ f xs = [ ((f t1, fmap f t2), x) | ((t1,t2),x) <- xs] 

-- | Multiply all times by a common factor
--
-- This makes all intervals start later and increase all durations.
slow :: Rational -> Media a -> Media a
slow s (Media d xs) = Media (fmap (*s) d) $ mapTimes_ (*s) xs

-- | Divide all times by a common factor.
--
-- This makes all intervals start earlier and decreases all durations.
hasten :: Rational -> Media a -> Media a
hasten s = slow (1/s)

-- | Multiply all interval durations by a common factor.
--
-- This does not affect starting times or the total duration.
sustain :: Rational -> Media a -> Media a
sustain s (Media d xs) = Media d [((t1, fmap (f t1) s2), x) | ((t1,s2), x) <- xs]
    where f t1 t2 = t1 + s*(t2 - t1)

-- | Shift all times by a time difference.
-- The assigned duration will *not* be shifted.
--
-- Note: The intervals will be cut off if the starting time is negative.
shift :: Time -> Media a -> Media a
shift dt = trim (0, Nothing) . shift_ dt

shift_ dt (Media d xs) = Media d $ mapTimes_ (+dt) xs

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
        Just dt -> subtractTime dt d
    ys2 = case snd dt of
        Nothing -> []
        Just t2 -> [(dy, x) | (dx, x) <- xs, Just dy <- [intersection (t2,Nothing) dx]]
    ys1 = [(dy, x) | (dx, x) <- xs1, Just dy <- [intersection dt dx]]
    xs1 = takeWhile (not . earlier dt . fst) xs

-- | Separate intervals into those that start before a given time @t@ and
-- those that start later.
splitTime :: Time -> Media a -> (Media a, Media a)
splitTime t (Media d xs) =
    (Media (Just t) ys, Media (subtractTime t d) (mapTimes_ (subtract t) zs))
    where
    (ys, zs) = List.span ((< t) . start . fst) xs

-- | Subtract a given amount of time from a potentiall infinite interval
subtractTime :: Time -> Maybe Time -> Maybe Time
subtractTime t Nothing  = Nothing
subtractTime t (Just s) = Just $ max 0 (s-t)

-- | Replace each value with a sequence of intervals by itself.
-- The duration of the result is the duration of the first argument.
-- 
-- Very similar to the monadic bind '(>>=)', but does not satisfy the monad laws,
-- and is not compatible with the 'Applicative' instance.
bind :: Media a -> (a -> Media b) -> Media b
bind (Media d xs) g =  Media d $ concat [ y | Media _ y <- ys ]
    where
    ys = [ trim dt $ shift_ (start dt) (g a) | (dt,a) <- xs ]

-- | Synonym for 'bind' with arguments flipped.
adorn :: (a -> Media b) -> Media a -> Media b
adorn = flip bind

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
        = Media (maxMaybe dx dy) $ merge (comparing $ start . fst) xs ys

-- | Sequential composition.
instance Semigroup (Media a) where
    x@(Media dx xs) <> (Media dy ys) = case dx of
        Nothing -> x
        Just dx -> Media (fmap (+dx) dy) (xs ++ mapTimes_ (+dx) ys)

-- | Repeat a 'Media' in infinite sequence.
cycle :: Media a -> Media a
cycle x = Media Nothing $ toIntervals y
    where y = x <> cycle x

-- | Sequential composition.
instance Monoid (Media a) where
    mempty  = empty
    mappend = (<>)

{-----------------------------------------------------------------------------
  Helper functions
------------------------------------------------------------------------------}
-- | Take the maximum of two maybes. 'Nothing' represents infinity.
maxMaybe :: Maybe Time -> Maybe Time -> Maybe Time
maxMaybe (Just a) (Just b) = Just (max a b)
maxMaybe _        _        = Nothing

-- | Take the minimum of two maybes. 'Nothing' represents infinity.
minMaybe :: Maybe Time -> Maybe Time -> Maybe Time
minMaybe = unionWith min

-- | Merge two sorted lists into a sorted list
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp []     ys     = ys
merge cmp xs     []     = xs
merge cmp (x:xs) (y:ys) = case cmp x y of
    LT -> x : merge cmp xs (y:ys)
    EQ -> x : y : merge cmp xs ys   -- EQ may be an equivalence only
    GT -> y : merge cmp (x:xs) ys

-- | Combine two maybe values.
unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith f Nothing  y        = y
unionWith f x        Nothing  = x
unionWith f (Just x) (Just y) = Just (f x y)


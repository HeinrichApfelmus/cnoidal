{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Cnoidal.Media where

import qualified Data.Char         as Char
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
type Time = Rational

{-----------------------------------------------------------------------------
    Intervals
------------------------------------------------------------------------------}
-- | Intervals that start at some point in time, but may never end.
type Interval = (Time, Maybe Time)

start :: Interval -> Time
start (t1,_) = t1

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
intersection (t1,t2) (s1,s2) = legalize (max t1 s1, unionWith min t2 s2)
    where
    legalize (t1,Just t2) | t1 >= t2 = Nothing
    legalize x                       = Just x

-- | Test whether two intervals intersect.
-- Here, this does not include touching at the endpoints.
intersect :: Interval -> Interval -> Bool
intersect a b = isJust $ intersection a b

-- | Test whether two intervals are disjoint.
disjoint t s = not $ intersect t s

{-----------------------------------------------------------------------------
    Media
------------------------------------------------------------------------------}
-- | 'Media' denotes a collection of time intervals with associated values.
newtype Media a = M [(Interval, a)]
        -- Invariant: All starting times are >= 0.
        -- Invariant: Starting times of intervals appear in order
    deriving (Eq, Ord, Show)

-- | Return list of intervals and values.
toIntervals :: Ord a => Media a -> [(Interval, a)]
toIntervals (M xs) = xs

instance Functor Media where
    fmap f (M xs) = M $ map (\(a,x) -> (a,f x)) xs

-- | Sequence of intervals of unit length whose values are from the given list
fromList :: [a] -> Media a
fromList = M . map (\(k,x) -> ((k,Just $ k+1),x)) . zip [0..]

fromInterval :: (Interval, a) -> Media a
fromInterval = M . (:[])

-- | Smallest interval that contains the whole Media
envelope :: Media a -> Interval
envelope (M xs) = (minimum (map (fst.fst) xs), maximum (map (snd.fst) xs))
    where
    maximum = foldr (unionWith max) (Just 0)

-- | Keep only those intervals where the value satisfies a predicate
filter :: (a -> Bool) -> Media a -> Media a
filter p (M xs) = M $ List.filter (p . snd) xs

-- | Do *not* export this function, as it can break the invariant.
mapTimes_ :: (Time -> Time) -> [(Interval, a)] -> [(Interval, a)]
mapTimes_ f xs = [ ((f t1, fmap f t2), x) | ((t1,t2),x) <- xs] 

-- | Multiply all times by a common factor, making the intervals longer.
slow :: Rational -> Media a -> Media a
slow s (M xs) = M $ mapTimes_ (*s) xs

-- | Divide all times by a common factor, making the intervals shorter.
hasten :: Rational -> Media a -> Media a
hasten s (M xs) = M $ mapTimes_ (/s) xs

-- | Shift all times by a time difference.
-- Note that the intervals will be cut off if the starting time is negative.
shift :: Time -> Media a -> Media a
shift dt = trim (0, Nothing) . shift_ dt

shift_ dt (M xs) = M $ mapTimes_ (+dt) xs

-- | Repeatedly shift a sequence of media and stack the in parallel.
staircase :: Time -> [Media a] -> Media a
staircase dt xs = asum [(shift (fromIntegral n) x) | (x,n) <- zip xs [0..]]
    where asum = foldr (<|>) empty

-- | Trim a Media to a time interval.
trim :: Interval -> Media a -> Media a
trim dt = fst . cut dt

-- | Cut a Media into two pieces according to a time interval.
-- The second Media will be shifted to start at 0.
cut :: Interval -> Media a -> (Media a, Media a)
cut dt (M xs) = (M ys1, M ys2)
    where
    ys2 = case snd dt of
        Nothing -> []
        Just t2 -> [(dy, x) | (dx, x) <- xs, Just dy <- [intersection (t2,Nothing) dx]]
    ys1 = [(dy, x) | (dx, x) <- xs1, Just dy <- [intersection dt dx]]
    xs1 = takeWhile (not . earlier dt . fst) xs

-- TODO: Implement function spanInterval that works with an interval
-- This can then be used with `unfold` !

instance Monad Media where
    return x       = M [ ((0,Nothing),x) ]
    (M xs) >>= f = M $ concat [ y | M y <- ys ]
        where
        ys = [ trim dt $ shift_ (start dt) (f a) | (dt,a) <- xs ]

instance MonadPlus Media where
    mplus = (<|>)

instance Alternative Media where
    empty = M []
    (M xs) <|> (M ys) = M $ List.sortBy (comparing (fst.fst)) $ xs ++ ys

-- | Turn lists of values into multiple intervals.
polyphony :: Media [a] -> Media a
polyphony (M xs) = M [(t,a) | (t,as) <- xs, a <- as]


-- | Intersect intervals pairwise and apply function.
apply :: [(Interval, a -> b)] -> [(Interval, a)] -> [(Interval, b)]
apply []          _  = []
apply ((tf,f):fs) xs = [(x,y) | (Just x,y) <- results] ++ apply fs xs'
    where
    results = takeWhile (isJust . fst) [ (intersection tf tx, f x) | (tx, x) <- xs']
    xs'     = dropWhile (later tf . fst) xs

instance Applicative Media where
    pure = return
    (M fs) <*> (M xs) = M $ apply fs xs

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

-- | Combine two maybe values.
unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith f Nothing  y        = y
unionWith f x        Nothing  = x
unionWith f (Just x) (Just y) = Just (f x y)


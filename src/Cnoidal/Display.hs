{-# LANGUAGE BangPatterns, TypeFamilies #-}
module Cnoidal.Display where

import qualified Data.Map         as Map

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg               as SVG

import Hyper
import Hyper.Extra

import Cnoidal.Media

{-----------------------------------------------------------------------------
    Display
------------------------------------------------------------------------------}
-- | Display 'Media' as a rectangular boxes on a timeline.
timeline :: Ord a => Media a -> Graphic
timeline m = dia $ rectangles $ zip is xs
    where
    (is, as) = unzip $ toIntervals m
    xs       = uniques as

rectangles :: [(Interval, Integer)] -> QDiagram SVG V2 Double Any
rectangles = position . map (uncurry toDia)
    where
    height = 0.1 :: Double
    toDia (t1, mt2) y =
        ( p2   (a + (b-a)/2, (height * fromIntegral (y-1)))
        , rect (b-a) height # lw 2)
        where
        a = fromRational t1
        b = maybe 20 fromRational mt2

{-----------------------------------------------------------------------------
    Utilities
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


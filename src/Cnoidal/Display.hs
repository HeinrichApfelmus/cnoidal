{-# LANGUAGE BangPatterns, TypeFamilies, FlexibleContexts #-}
module Cnoidal.Display where

import qualified Data.List
import qualified Data.Map         as Map

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Hyper
import Hyper.Extra                        (fromSvg)

import Cnoidal.Media

type Dia = QDiagram SVG V2 Double Any

{-----------------------------------------------------------------------------
    Integrate `diagrams-svg` and `svg-builder` wiht HyperHaskell
------------------------------------------------------------------------------}
dia :: Dia -> Graphic
dia = fromSvg . renderDia SVG (SVGOptions absolute Nothing mempty [] True)

{-----------------------------------------------------------------------------
    Display
------------------------------------------------------------------------------}
-- | Display 'Media' as a rectangular boxes on a timeline.
timeline :: (Show a, Ord a) => Media a -> Graphic
timeline = dia . pad 1.1 . translateX (-10) . scale 24
     . vcat . map drawRow . groupRows

groupRows :: Ord a => Media a -> [(a, [Interval])]
groupRows media = [(a, map fst $ Prelude.filter ((== a) . snd) ias) | a <- as]
    where
    ias = toIntervals media
    as  = reverse $ Data.List.nub $ Data.List.sort $ map snd ias

drawRow :: Show a => (a, [Interval]) -> Dia
drawRow (a,xs) = position $ title : map toRect xs
    where
    title = (p2 (-1.7,0), (text (show a) # scale 0.8) `atop`
                rect 3 1 # fc (darken 0.6 white) # lw 0)
    toRect (t1, mt2) = (p2 (a + (b-a)/2, 0), rect (b-a) 1 # lw 1)
        where
        a = 8*fromRational t1
        b = 8*maybe 20 fromRational mt2

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


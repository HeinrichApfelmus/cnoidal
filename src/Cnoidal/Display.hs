{-# LANGUAGE BangPatterns, TypeFamilies, FlexibleContexts #-}
module Cnoidal.Display where

import qualified Data.List
import qualified Data.Map         as Map

import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Graphics.Svg     as SVG

import Hyper
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as TL

import Cnoidal.Media

type Dia = QDiagram SVG V2 Double Any

{-----------------------------------------------------------------------------
    Integrate `diagrams-svg` and `svg-builder` wiht HyperHaskell
------------------------------------------------------------------------------}
dia :: Dia -> Graphic
dia = html . TL.toStrict . SVG.renderText
    . renderDia SVG (SVGOptions absolute Nothing (T.pack "") [] True)
    -- (mkWidth 250)

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
drawRow (a,xs) = lw 1 $ position $
    (p2 (-3/2, 0), (text (show a) # scale 0.8) `atop` rect 3 1) : map toRect xs
    where
    toRect (t1, mt2) = (p2 (a + (b-a)/2, 0), rect (b-a) 1)
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


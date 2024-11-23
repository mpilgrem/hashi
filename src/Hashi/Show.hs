{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|
Module      : Hashi.Show
Description : A displayer of a Hashiwokakero solution
Copyright   : Copyright 2024 Mike Pilgrem
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Show
  ( Draw (..)
  , coordsToIsland
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )
import           Diagrams.Backend.Rasterific ( B )
import           Diagrams.Prelude
                   ( AlphaColour, Diagram, V2 (..), (#), black, circle, fc, fcA, fontSizeL
                   , fromVertices, lc, lw, lwO, none, opaque, p2, rect, text
                   , translate, transparent, wheat, white
                   )
import           Hashi.Types
                   ( BridgeSet (..), Field (..), Index, Island, IslandState (..)
                   , Problem, State
                   )
import           Constants
                   ( backgroundHeight', backgroundWidth', cellDim', heightGrid
                   , margin', radius', widthGrid
                   )

class Draw a where

  draw :: a -> Diagram B

instance Draw Problem where

  draw = drawMap draw Nothing

instance Draw (Index, Field) where

  draw (_, Water) = mempty
  draw (i, (Island n)) = drawIsland transparent i n

instance Draw State where

  draw = drawMap draw (Just drawBridges)

instance Draw Island where

  draw (i, islandState) = drawIsland (opaque wheat) i (iConstraint islandState)

drawMap ::
     ((Index, a) -> Diagram B)
  -> Maybe ((Index, a) -> Diagram B)
  -> Map.Map Index a
  -> Diagram B
drawMap drawItem mDrawBridges m =
     mconcat
       (  map drawItem items
       <> maybe [] (\f -> map f items) mDrawBridges
       )
  <> backdrop
 where
  items = Map.assocs m

drawIsland :: AlphaColour Double -> Index -> Int -> Diagram B
drawIsland c (col, row) n =
  (  text (show n) # fontSizeL radius'
  <> circle radius' # fcA c
  ) # translate (V2 x y)
 where
  x = fromIntegral col * cellDim' + cellDim' / 2.0
  y = fromIntegral row * cellDim' + cellDim' / 2.0

drawBridges :: Island -> Diagram B
drawBridges ((c, r), islandState) = right <> down
 where
  right = drawBridge rightB rightNeighbor
  down = drawBridge bottomB bottomNeighbor
  drawBridge dir neighbor = case iBridgeSets islandState of
    [bridgeSet] -> case dir bridgeSet of
      1 -> line c r c' r' # lwO 2 # lc black
      2 ->    line c r c' r' # lwO 2 # lc white
           <> line c r c' r' # lwO 6 # lc black
      _ -> mempty
    _ -> error "Not a solution"
   where
    (c', r') = fromMaybe (error "No neighbour") (neighbor islandState)

line :: Int -> Int -> Int -> Int -> Diagram B
line x1 y1 x2 y2 = fromVertices [p2 (x1', y1'), p2 (x2', y2')]
 where
  x1' = fromIntegral x1 * cellDim' + cellDim'/2.0
  y1' = fromIntegral y1 * cellDim' + cellDim'/2.0
  x2' = fromIntegral x2 * cellDim' + cellDim'/2.0
  y2' = fromIntegral y2 * cellDim' + cellDim'/2.0

backdrop :: Diagram B
backdrop = translate (V2 x y) (rect w h # fc wheat # lw none)
 where
  w = backgroundWidth'
  h = backgroundHeight'
  x = w / 2.0 - margin'
  y = h / 2.0 - margin'

coordsToIsland :: Double -> Double -> Maybe (Int, Int)
coordsToIsland x y =
  if col < 0 || row < 0 || col >= widthGrid || row >= heightGrid
    then Nothing
    else Just (row, col)
 where
  x' = (x - margin') / cellDim'
  y' = (backgroundHeight' - y - margin') / cellDim'
  col = floor x'
  row = floor y'

{-# LANGUAGE NoMonomorphismRestriction #-}

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
  ( showState
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )
import           Diagrams.Backend.Rasterific ( B )
import           Diagrams.Prelude
                   ( Diagram, V2 (..), (#), black, circle, fc, fontSizeL
                   , fromVertices, lc, lw, lwO, none, p2, rect, text
                   , translate, wheat, white
                   )
import           Hashi.Types ( BridgeSet (..), Island, IslandState (..), State )

import           Constants
                   ( backgroundHeight', backgroundWidth', cellDim', margin'
                   , radius'
                   )

showState :: State -> Diagram B
showState state =
  mconcat (map island islands <> map bridges islands) <> backdrop
 where
  islands = Map.assocs state

backdrop :: Diagram B
backdrop = translate (V2 x y) (rect w h # fc wheat # lw none)
 where
  w = backgroundWidth'
  h = backgroundHeight'
  x = w / 2.0 - margin'
  y = h / 2.0 - margin'

island :: Island -> Diagram B
island ((row, col), islandState) =
  (  text (show $ iConstraint islandState) # fontSizeL radius'
  <> circle radius' # fc wheat
  ) # translate (V2 x y)
 where
  x = fromIntegral col * cellDim' + cellDim' / 2.0
  y = fromIntegral row * cellDim' + cellDim' / 2.0

bridges :: Island -> Diagram B
bridges ((r, c), islandState) = right <> down
 where
  right = bridge rightB rightNeighbor
  down = bridge bottomB bottomNeighbor
  bridge dir neighbor = case iBridgeSets islandState of
    [bridgeSet] -> case dir bridgeSet of
      1 -> line c r c' r' # lwO 2 # lc black
      2 ->    line c r c' r' # lwO 2 # lc white
           <> line c r c' r' # lwO 6 # lc black
      _ -> mempty
    _ -> error "Not a solution"
   where
    (r', c') = fromMaybe (error "No neighbour") (neighbor islandState)

line :: Int -> Int -> Int -> Int -> Diagram B
line x1 y1 x2 y2 = fromVertices [p2 (x1', y1'), p2 (x2', y2')]
 where
  x1' = fromIntegral x1 * cellDim' + cellDim'/2.0
  y1' = fromIntegral y1 * cellDim' + cellDim'/2.0
  x2' = fromIntegral x2 * cellDim' + cellDim'/2.0
  y2' = fromIntegral y2 * cellDim' + cellDim'/2.0

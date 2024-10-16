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
import           Diagrams.Backend.SVG ( B )
import           Diagrams.Prelude
                   ( Diagram, (#), black, circle, fc, font, fontSizeL, frame
                   , fromVertices, lc, lwO, p2, sRGB, text, translateX
                   , translateY, white
                   )
import           Hashi.Types ( BridgeSet (..), Island, IslandState (..), State )

showState :: State -> Diagram B
showState state = frame 1 $ mconcat (map island islands <> map bridges islands)
 where
  islands = Map.assocs state

island :: Island -> Diagram B
island ((r, c), islandState) =
  (  text (show $ iConstraint islandState) # fontSizeL 0.8 # font "Arial"
  <> circle 0.45 # fc (sRGB 0.8 0.9 1.0)
  ) # translateX c' # translateY r' # lwO 0.5
 where
  c' = fromIntegral c
  r' = fromIntegral r

bridges :: Island -> Diagram B
bridges ((r, c), islandState) = right <> down
 where
  right = bridge rightB rightNeighbor
  down = bridge bottomB bottomNeighbor
  bridge fB fN = case iBridgeSets islandState of
    [bridgeSet] -> case fB bridgeSet of
      1 -> line c r c' r' # lwO 2 # lc black
      2 ->    line c r c' r' # lwO 2 # lc white
           <> line c r c' r' # lwO 6 # lc black
      _ -> mempty
    _ -> error "Not a solution"
   where
    (r', c') = case fN islandState of
      Just p -> p
      Nothing -> error "No neighbour"

line :: Int -> Int -> Int -> Int -> Diagram B
line x1 y1 x2 y2 = fromVertices [p2 (x1', y1'), p2 (x2', y2')]
 where
  x1' = fromIntegral x1
  y1' = fromIntegral y1
  x2' = fromIntegral x2
  y2' = fromIntegral y2

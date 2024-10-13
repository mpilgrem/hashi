{-|
Module      : Hashi.State
Description : Convert a Hashiwokakero problem to an initial state
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.State
  ( stateFromProblem
  ) where

import           Data.Array.IArray ( (!), assocs, bounds )
import           Data.List ( find )
import qualified Data.Map as Map
import           Data.Maybe ( isJust, mapMaybe )
import           Hashi.Types
                   ( BridgeSet (..), Field (..), Index, Island, IslandState (..)
                   , Problem, State, isIsland
                   )

-- | Yields an initial state from a problem. Assumes that the top left location
-- in the grid is @(0, 0)@.
stateFromProblem :: Problem -> State
stateFromProblem p = state
 where
  (rn, cn) = case bounds p of
    ((0, 0), corner) -> corner
    (_, _) -> error "Lower bound not (0, 0)"
  state = Map.fromList $ mapMaybe toIsland $ assocs p
  islands = Map.assocs state

  toIsland :: (Index, Field) -> Maybe Island
  toIsland (i, Island n) = Just (i, islandState)
   where
    islandState =
      IslandState n (top i) (right i) (bottom i) (left i) rx bx bridgeSets
    bridgeSets = filter allBridgesToSomewhere $ nBridgeSets n
    allBridgesToSomewhere (BridgeSet t r b l) =
         (isJust (topNeighbor islandState) || t == 0)
      && (isJust (rightNeighbor islandState) || r == 0)
      && (isJust (bottomNeighbor islandState) || b == 0)
      && (isJust (leftNeighbor islandState) || l == 0)
    rx = map fst $ filter (xing (i, islandState)) islands
    bx = map fst $ filter (`xing` (i, islandState)) islands
  toIsland (_, Water) = Nothing

  top (r, c) = find isIslandIndex [(rr, c) | rr <- [r - 1, r - 2 .. 0]]
  right (r, c) = find isIslandIndex [(r, cc) | cc <- [c + 1 .. cn]]
  bottom (r, c) = find isIslandIndex [(rr, c) | rr <- [r + 1 .. rn]]
  left (r, c) = find isIslandIndex [(r, cc) | cc <- [c - 1, c - 2 .. 0]]
  isIslandIndex i = isIsland (p!i)

-- | Would a bridge to the righthand neighbour of the first island (if any)
-- cross a bridge to the bottom neighbour of the second island (if any)?
xing :: Island -> Island -> Bool
xing ((r1, c1), s1) ((r2, c2), s2) =
  case (rightNeighbor s1, bottomNeighbor s2) of
    (Just (_, c1'), Just(r2', _)) -> r2 < r1 && r1 < r2' && c1 < c2 && c2 < c1'
    _                        -> False

-- | A list of all possible bridge sets consistent with the given constraint.
nBridgeSets :: Int -> [BridgeSet]
nBridgeSets n = filter p allBridgeSets
 where
  p (BridgeSet t r b l) = t + r + b + l == n

  -- A list of all possible bridge sets.
  allBridgeSets :: [BridgeSet]
  allBridgeSets =
    [ BridgeSet t r b l
    | t <- [0 .. 2]
    , r <- [0 .. 2]
    , b <- [0 .. 2]
    , l <- [0 .. 2]
    ]

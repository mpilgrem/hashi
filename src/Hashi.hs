{-|
Module      : Hashi
Description : A solver for Hashiwokakero puzzles
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi
  ( solveProblem
  ) where

import           Data.List ( find )
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map as Map
import           Data.Maybe ( mapMaybe )
import qualified Data.Set as Set
import           Data.Set ( Set )
import           Hashi.State ( stateFromProblem )
import           Hashi.Types
                   ( BridgeSet (..), Index, Island, IslandState (..), Problem
                   , State, isUncertain
                   )

solveProblem :: Problem -> [State]
solveProblem p = concatMap solveState $ narrowAll state
 where
  state = stateFromProblem p

solveState :: State -> [State]
solveState state =
  case (connectedComponents state, find uncertain islands) of
    -- Nothing uncertain and one set of connected components: solved
    (_ :| [], Nothing) -> [state]
    -- Otherwise, if nothing uncertain: no solution
    (_, Nothing) -> []
    -- Something uncertain and one set of connected components: try to solve
    (_ :| [], Just island) -> solveForIsland state island
    -- Otherwise, if something uncertain ...
    (ccs, Just island)  -> if all unfinished ccs
      -- All of the sets of connected components are unfinished: try to solve
      then solveForIsland state island
      -- One or more of the sets of connected components are finished: no
      -- solution
      else []
 where
  islands = Map.assocs state
  uncertain (_, islandState) = isUncertain islandState
  unfinished = any (isUncertain . (state Map.!))

solveForIsland :: State -> Island -> [State]
solveForIsland state island@(_, islandState) =
  concatMap tryBridgeSet $ iBridgeSets islandState
 where
  tryBridgeSet bridgeSet =
    [ newState
    | newStates <- narrow locs $ setBridgeSets state island [bridgeSet]
    , newState <- solveState newStates
    ]
  locs = locsFromIslandState islandState

narrow :: Set Index -> State -> [State]
narrow locs state
  | Set.null locs = [state]
  | null validBridgeSets = []
  | bridgeSets == validBridgeSets = narrow remainingLocs state
  | otherwise =
      let newLocs = locsFromIslandState islandState
      in  narrow
            (Set.union remainingLocs newLocs)
            (setBridgeSets state island validBridgeSets)
 where
  (loc, remainingLocs) = Set.deleteFindMin locs
  islandState = state Map.! loc
  island = (loc, islandState)
  bridgeSets = iBridgeSets islandState
  validBridgeSets =
      filter (noXings rightB rightXings bottomB)
    $ filter (noXings bottomB bottomXings rightB)
    $ filter (match topB topNeighbor bottomB)
    $ filter (match rightB rightNeighbor leftB)
    $ filter (match bottomB bottomNeighbor topB)
    $ filter (match leftB leftNeighbor rightB) bridgeSets
  match thisB neighbor otherB b = case neighbor islandState of
    Nothing -> True
    Just neighborLoc -> thisB b `elem` map otherB (getBridgeSets neighborLoc)
  noXings thisB others otherB b =
       thisB b == 0
    || all
         (\o -> 0 `elem` map otherB (getBridgeSets o))
         (others islandState)
  getBridgeSets :: Index -> [BridgeSet]
  getBridgeSets l = iBridgeSets $ state Map.! l

setBridgeSets :: State -> Island -> [BridgeSet] -> State
setBridgeSets state (loc, islandState) bridgeSets =
  Map.insert loc (islandState {iBridgeSets = bridgeSets}) state

locsFromIslandState :: IslandState -> Set Index
locsFromIslandState islandState =
  Set.fromList $ neighbours <> rightXings islandState <> bottomXings islandState
 where
  neighbours = mapMaybe
    ($ islandState)
    [topNeighbor, rightNeighbor, bottomNeighbor, leftNeighbor]

narrowAll :: State -> [State]
narrowAll state = narrow (Map.keysSet state) state

connectedComponents :: State -> NonEmpty (Set Index)
connectedComponents state =
  let (seed, remainingUnvisited) = Set.deleteFindMin (Map.keysSet state)
  in  cc (Set.empty :| []) (Set.singleton seed) remainingUnvisited
 where
  cc :: NonEmpty (Set Index) -> Set Index -> Set Index -> NonEmpty (Set Index)
  cc ccs@(cs :| rest) locs unvisited
    | Set.null locs = if Set.null unvisited
        then
          ccs
        else
          let (seed, remainingUnvisited) = Set.deleteFindMin unvisited
          in  cc (Set.empty <| ccs) (Set.singleton seed) remainingUnvisited
    | otherwise =
        let (seed, remainingLocs) = Set.deleteFindMin locs
            remainingUnvisited = Set.delete seed unvisited
            islandState = state Map.! seed
            conn = Set.fromList $ mapMaybe
              (($ islandState) . snd)
              ( filter
                  f
                  [ (topB, topNeighbor)
                  , (rightB, rightNeighbor)
                  , (bottomB, bottomNeighbor)
                  , (leftB, leftNeighbor)
                  ]
              )
            f (fB, _) = 0 `notElem` map fB (iBridgeSets islandState)
            newCs = Set.insert seed cs :| rest
            newLocs = Set.union remainingLocs conn Set.\\ cs
        in  cc newCs newLocs remainingUnvisited

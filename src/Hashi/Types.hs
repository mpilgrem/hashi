{-|
Module      : Hashi.Types
Description : Types representing Hashiwokakero problems and solutions
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Types
  ( Index
  , Field (..)
  , isIsland
  , Problem
  , BridgeSet (..)
  , GetBridge
  , IslandState (..)
  , isUncertain
  , State
  , Island
  ) where

import           Data.Array.IArray ( Array )
import qualified Data.Map as Map

-- | Type synonym representing locations on the grid.
type Index = (Int, Int)

-- | Type representing fields on the puzzle grid.
data Field
  = Water
  | Island Int
  deriving (Eq)

instance Show Field where
  show Water = "."
  show (Island x) = show x

isIsland :: Field -> Bool
isIsland (Island _) = True
isIsland _          = False

-- | Type synonym representing puzzles.
type Problem = Array Index Field

-- | Type representing the bridges of an island.
data BridgeSet = BridgeSet
  { topB :: Int
  , rightB :: Int
  , bottomB :: Int
  , leftB :: Int
  } deriving (Eq, Show)

-- | Type synonyn representing functions obtaining a brige from the brides of an
-- Island.
type GetBridge = BridgeSet -> Int

-- | Type representing states of an island.
data IslandState = IslandState
  { iConstraint :: Int
  , topNeighbor :: Maybe Index
  , rightNeighbor :: Maybe Index
  , bottomNeighbor :: Maybe Index
  , leftNeighbor :: Maybe Index
  , rightXings :: [Index]
    -- ^ Islands whose bottom bridges cross with our right.
  , bottomXings :: [Index]
    -- ^ Islands whose right bridges cross with our bottom.
  , iBridgeSets :: [BridgeSet]
    -- ^ A list of possible bridges for the island.
  } deriving (Eq, Show)

isUncertain :: IslandState -> Bool
isUncertain islandState = length (iBridgeSets islandState) > 1

-- | Type synonym representing states of all islands.
type State = Map.Map Index IslandState

-- | Type synonym representing islands.
type Island = (Index, IslandState)

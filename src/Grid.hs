{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

{-|
Module      : Grid
Description : Functions manipulating a Hashiwokakero puzzle
Copyright   : Copyright 2024 Mike Pilgrem
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Grid
  ( emptyProblem
  , updateGrid
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )

import           Hashi.Types ( Field (..), Problem (..) )

emptyProblem :: Int -> Int -> Problem
emptyProblem widthGrid heightGrid = Problem widthGrid heightGrid Map.empty

getCell :: Problem -> Int -> Int -> Field
getCell problem col row =
  fromMaybe Water $ Map.lookup (col, row) (pGrid problem)

setCell :: Int -> Int -> Field -> Problem -> Problem
setCell col row field problem = problem
  { pGrid = case field of
              Water -> Map.delete (col, row) grid
              island -> Map.insert (col, row) island grid
  }
 where
  grid = pGrid problem

clearCells :: Int -> Int -> Problem -> Problem
clearCells col row problem =
  clearUp $ clearDown $ clearLeft $ clearRight problem
 where
  widthGrid = pWidthGrid problem
  heightGrid = pHeightGrid problem
  clearUp = if row + 1 >= heightGrid
    then id
    else setCell col (row + 1) Water
  clearDown = if row <= 0
    then id
    else setCell col (row - 1) Water
  clearRight = if col + 1 >= widthGrid
    then id
    else setCell (col + 1) row Water
  clearLeft = if col <= 0
    then id
    else setCell (col - 1) row Water

cycleCell :: Field -> Field
cycleCell Water = Island 1
cycleCell (Island 8) = Water
cycleCell (Island n) = Island (n + 1)

updateGrid :: Int -> Int -> Problem -> Problem
updateGrid col row oldProblem =
  let oldCell = getCell oldProblem col row
      newCell = cycleCell oldCell
      problem = setCell col row newCell oldProblem
  in  case newCell of
        Water -> problem
        Island _ -> clearCells col row problem

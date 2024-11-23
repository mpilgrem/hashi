{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Grid
  ( emptyGrid
  , updateGrid
  ) where

import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe )

import           Constants ( heightGrid, widthGrid )
import           Hashi.Types ( Field (..), Problem )

emptyGrid :: Problem
emptyGrid = Map.empty

getCell :: Problem -> Int -> Int -> Field
getCell grid col row =
  fromMaybe Water $ Map.lookup (col, row) grid

setCell :: Int -> Int -> Field -> Problem -> Problem
setCell col row field = case field of
  Water -> Map.delete (col, row)
  island -> Map.insert (col, row) island

clearCells :: Int -> Int -> Problem -> Problem
clearCells col row = clearUp . clearDown . clearLeft . clearRight
 where
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
updateGrid col row oldGrid =
  let oldCell = getCell oldGrid col row
      newCell = cycleCell oldCell
      gameGrid = setCell col row newCell oldGrid
  in  case newCell of
        Water -> gameGrid
        Island _ -> clearCells col row gameGrid

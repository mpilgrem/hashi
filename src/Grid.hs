{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Grid
  ( Grid
  , Cell
  , widthGrid
  , heightGrid
  , emptyGrid
  , updateGrid
  ) where

import qualified Data.Vector as V
import           Data.Vector ( Vector, (!) )
import qualified Data.Vector.Mutable as MV

import           Constants ( heightGrid, widthGrid )

type Cell = Maybe Int

type Grid = Vector (Vector Cell)

emptyGrid :: Grid
emptyGrid = V.replicate heightGrid (V.replicate widthGrid Nothing)

getCell :: Grid -> Int -> Int -> Cell
getCell grid col row = grid ! row ! col

setCell :: Int -> Int -> Cell -> Grid -> Grid
setCell col row cell = V.modify
  (\v -> do
    rowV <- MV.read v row
    MV.write v row (setRow col cell rowV)
  )

clearCells :: Int -> Int -> Grid -> Grid
clearCells col row = clearUp . clearDown . clearLeft . clearRight
 where
  clearUp = if row + 1 >= heightGrid
    then id
    else setCell col (row + 1) Nothing
  clearDown = if row <= 0
    then id
    else setCell col (row - 1) Nothing
  clearRight = if col + 1 >= widthGrid
    then id
    else setCell (col + 1) row Nothing
  clearLeft = if col <= 0
    then id
    else setCell (col - 1) row Nothing

setRow :: Int -> Cell -> Vector Cell -> Vector Cell
setRow col cell = V.modify (\v -> MV.write v col cell)

cycleCell :: Cell -> Cell
cycleCell Nothing = Just 1
cycleCell (Just 8) = Nothing
cycleCell (Just n) = Just (n + 1)

updateGrid :: Int -> Int -> Grid -> Grid
updateGrid col row oldGrid =
  let oldCell = getCell oldGrid col row
      newCell = cycleCell oldCell
      gameGrid = setCell col row newCell oldGrid
  in  case newCell of
        Nothing -> gameGrid
        Just _ -> clearCells col row gameGrid

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Draw
  ( drawGrid
  , backgroundWidth
  , backgroundHeight
  , coordsToIsland
  ) where

import           Data.Vector ( Vector )
import           Diagrams.Backend.Rasterific ( B )
import           Diagrams.Prelude
                   ( Diagram, V2 (..), (#), circle, fc, fontSizeL, imap, lw
                   , none, rect, text, translate, wheat
                   )
import           Grid ( Cell, Grid, heightGrid, widthGrid )

radius :: Int
radius = 20

radius' :: Double
radius' = fromIntegral radius

margin :: Int
margin = radius `div` 4

margin' :: Double
margin' = fromIntegral margin

cellDim :: Int
cellDim = 2 * (radius + margin)

cellDim' :: Double
cellDim' = fromIntegral cellDim

backgroundWidth :: Int
backgroundWidth = widthGrid * cellDim + 2 * margin

backgroundWidth' :: Double
backgroundWidth' = fromIntegral backgroundWidth

backgroundHeight :: Int
backgroundHeight = heightGrid * cellDim + 2 * margin

backgroundHeight' :: Double
backgroundHeight' = fromIntegral backgroundHeight

drawCell :: Cell -> Diagram B
drawCell Nothing = mempty
drawCell (Just n) = text (show n) # fontSizeL radius' <> circle radius'

drawRow :: Int -> Vector Cell -> Diagram B
drawRow row cells =foldl (<>) mempty (imap shift cells)
 where
  shift :: Int -> Cell -> Diagram B
  shift col cell = translate (V2 x y) (drawCell cell)
   where
    x = fromIntegral col * cellDim' + cellDim' / 2.0
    y = fromIntegral row * cellDim' + cellDim' / 2.0

drawGrid :: Grid -> Diagram B
drawGrid grid = foldl (<>) mempty (imap drawRow grid) <> backdrop

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

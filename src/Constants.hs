module Constants
  ( widthGrid
  , heightGrid
  , radius
  , radius'
  , margin
  , margin'
  , cellDim
  , cellDim'
  , backgroundWidth
  , backgroundWidth'
  , backgroundHeight
  , backgroundHeight'
  ) where

widthGrid :: Int
widthGrid = 10

heightGrid :: Int
heightGrid = 6

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

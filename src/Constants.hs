module Constants
  ( widthGridDefault
  , heightGridDefault
  , radius
  , radius'
  , margin
  , margin'
  , cellDim
  , cellDim'
  ) where

widthGridDefault :: Int
widthGridDefault = 6

heightGridDefault :: Int
heightGridDefault = 4

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

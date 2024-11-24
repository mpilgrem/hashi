{-|
Module      : Constants
Description : Constants affecting display of Hashiwokakero puzzles
Copyright   : Copyright 2024 Mike Pilgrem
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

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

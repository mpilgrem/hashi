{-|
Module      : Hashi.Read
Description : A reader of a Hashiwokakero puzzle from a Grid
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Read
  ( gridToProblem
  ) where

import           Data.Array.IArray ( listArray )
import qualified Data.Vector as V
import           Hashi.Types ( Field (..), Problem )

import           Constants ( heightGrid, widthGrid )
import           Grid ( Cell, Grid )

gridToProblem :: Grid -> Problem
gridToProblem grid =
 let fields = map cellToField $ concatMap V.toList $ V.toList grid
 in  listArray ((0, 0), (heightGrid - 1, widthGrid - 1)) fields

cellToField :: Cell -> Field
cellToField Nothing = Water
cellToField (Just n) = Island n

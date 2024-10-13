{-|
Module      : Hashi.Read
Description : A reader of a Hashiwokakero puzzle from a string
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Read
  ( readProblem
  ) where

import           Control.Monad ( unless, when )
import           Data.Array.IArray ( listArray )
import           Data.Char ( digitToInt )
import qualified Data.Text as T
import           Data.Text ( Text )
import           Hashi.Types ( Field (..), Problem )

type ProblemList = [[Field]]

readProblem :: Text -> Either String Problem
readProblem t = do
  pl <- readProblemList t
  case pl of
    [] -> Left "Problem is empty."
    (p:_) -> do
      let columns = length p
      when (columns == 0) $
        Left "Problem starts with an empty line."
      unless (all ((== columns) . length) pl) $
        Left "Problem not rectangular."
      let rows = length pl
      pure $ listArray ((0, 0), (rows - 1, columns - 1)) $ concat pl

readProblemList ::  Text -> Either String ProblemList
readProblemList = (mapM . mapMText) readField . T.lines

readField :: Char -> Either String Field
readField '.' = Right Water
readField c
  | c >= '1' && c <= '8' = Right $ Island $ digitToInt c
  | otherwise = Left $ "Invalid character " <> (c : ".")

mapMText :: Monad m => (Char -> m a) -> Text -> m [a]
mapMText f = mapM f . T.unpack

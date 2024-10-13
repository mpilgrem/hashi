{-# LANGUAGE  QuasiQuotes      #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Hashi.Show
Description : A displayer of a hashiwokakero solution
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Hashi.Show
  ( showStateEPS
  ) where

import           Data.List ( nub )
import qualified Data.Text as T
import           Data.Text ( Text )
import qualified Data.Map as Map
import           Hashi.Types ( BridgeSet (..), Island, IslandState (..), State )
import           Text.Heredoc ( there )
import           TextShow

showStateEPS :: State -> Text
showStateEPS state =
     [there|hashiheader.eps|]
  <> T.concat (map bridges islands)
  <> T.concat (map circle islands)
 where
  islands = Map.assocs state

circle :: Island -> Text
circle ((r, c), islandState) = T.unwords
  [ showt r
  , showt c
  , showt (iConstraint islandState)
  , "circle\n"
  ]

bridges :: Island -> Text
bridges ((r, c), islandState) = right <> down
 where
  right = g rightB rightNeighbor
  down = g bottomB bottomNeighbor
  g fB fN = case nub $ map fB $ iBridgeSets islandState of
    [n] -> if n > 0
      then
        let (r', c') = case fN islandState of
              Just p -> p
              Nothing -> error "No neighbour"
        in  T.unwords
              [ showt r
              , showt c
              , showt r'
              , showt c'
              , showt n
              , "bridge\n"
              ]
      else ""
    _   -> ""

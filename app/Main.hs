{-# LANGUAGE LambdaCase #-}

{-|
Module      : Main
Description : A solver of Hashiwokakero puzzles
Copyright   : Copyright 2013 Harald Bögeholz
License     : BSD-2-Clause-Views
Maintainer  : public@pilgrem.com
Stability   : Experimental
Portability : Portable
-}

module Main
  ( main
  ) where

import qualified Data.Text.IO as T
import           Diagrams.Backend.SVG ( renderSVG )
import           Diagrams.TwoD ( dims2D )
import           Hashi ( solveProblem )
import           Hashi.Read ( readProblem )
import           Hashi.Show ( showState )
import           System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \case
  [filename] -> T.readFile filename >>= work (filename <> ".solution.svg")
  [] ->
    error
      "Usage: hashi-solve filename\nWill write solution to filename.solution.eps"
  _  -> error "Too many arguments."
 where
  size = dims2D 842.0 595.0
  work outfile s = case readProblem s of
    Left e -> putStrLn e
    Right p -> do
      putStrLn $ "Will write first solution to '" <> outfile <> "'."
      let solutions = solveProblem p
      case solutions of
        [] -> pure ()
        (solution:_) -> renderSVG outfile size $ showState solution
      putStrLn $ "Total number of solutions: " <> show (length solutions)

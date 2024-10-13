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
import           Hashi ( solveProblem )
import           Hashi.Read ( readProblem )
import           Hashi.Show ( showStateEPS )
import           System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \case
  [filename] -> T.readFile filename >>= work (filename <> ".solution.eps")
  [] ->
    error
      "Usage: hashi-solve filename\nWill write solution to filename.solution.eps"
  _  -> error "Too many arguments."
 where
  work outfile s = case readProblem s of
    Left e -> putStrLn e
    Right p -> do
      putStrLn $ "Will write first solution to '" <> outfile <> "'."
      let solutions = solveProblem p
      case solutions of
        [] -> pure ()
        (solution:_) -> T.writeFile outfile $ showStateEPS solution
      putStrLn $ "Total number of solutions: " <> show (length solutions)

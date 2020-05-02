module Main where

import System.Environment
import Sudoku
import Data.Maybe
import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies


main :: IO ()
main = do
  f : [] <- getArgs
  file       <- readFile f
  let problems   = lines file
      solutions = soln problems

  print (length (filter isJust solutions))



parallelMap :: (a -> b) -> [a] -> Eval [b]
parallelMap f [] = return []
parallelMap f (a:as) = do
  b  <- rpar (f a)
  bs <- parallelMap f as
  return (b:bs)

soln :: [String] -> [Maybe Sudoku.GridValues]
soln puzzles = runEval (parallelMap solve puzzles)

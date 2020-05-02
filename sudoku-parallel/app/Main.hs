module Main where

import System.Environment
import Sudoku
import Data.Maybe
import Data.Map
import qualified Data.Map as Map
import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies


main :: IO ()
main = do
  f : [] <- getArgs
  file       <- readFile f
  let problems = lines file
  let solutions = soln problems
  let mx = head solutions
  case mx of
        Nothing -> putStrLn "Failed to find a solution"
        Just x -> putStrLn "Succeeded"

parEval :: [String] -> [Maybe Sudoku.GridValues]
parEval puzzles =
  let (as,bs) = splitAt (length puzzles `div` 2) puzzles in
    runEval $ do
        as' <- rpar (force (map solve as))
        bs' <- rpar (force (map solve bs))
        rseq as'
        rseq bs'
        return (as' ++ bs')


parallelMap :: (a -> b) -> [a] -> Eval [b]
parallelMap f [] = return []
parallelMap f (a:as) = do
  b  <- rpar (f a)
  bs <- parallelMap f as
  return (b:bs)

soln :: [String] -> [Maybe Sudoku.GridValues]
soln puzzles = runEval (parallelMap solve puzzles)

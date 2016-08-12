-- | dynamic parallel sudoku solving

import Control.DeepSeq
import Control.Parallel.Strategies (rseq,rpar,runEval,Eval)
import Data.Maybe
import Sudoku
import System.Environment

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)


main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = runEval . parMap solve $ puzzles

  print . length . filter isJust $ solutions


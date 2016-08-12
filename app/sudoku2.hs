-- | static parallel sudoku solving

import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
import Data.Maybe
import Sudoku
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      (as,bs) = splitAt (length puzzles `div` 2) puzzles
      solutions = runEval $ do
        as' <- rpar $ force (solve <$> as)
        bs' <- rpar $ force (solve <$> bs)
        rseq as'
        rseq bs'
        return (as' ++ bs')

  print . length . filter isJust $ solutions

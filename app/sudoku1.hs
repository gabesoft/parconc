-- | sequential sudoku solving

import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = solve <$> puzzles

  print . length . filter isJust $ solutions

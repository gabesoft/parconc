-- | Fork example
module Main (main) where

import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100000 (putChar 'A'))
  forkIO (replicateM_ 100000 (putChar 'B'))
  forkIO (replicateM_ 100000 (putChar 'C'))
  replicateM_ 100000 (putChar 'D')
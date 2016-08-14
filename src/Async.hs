-- | Async helpers
module Async where

import Control.Concurrent
import Control.Exception

data Async a =
  Async (MVar a)

async :: IO a -> IO (Async a)
async action =
  do var <- newEmptyMVar
     forkIO $ action >>= putMVar var
     return $ Async var

wait :: Async a -> IO a
wait (Async var) = readMVar var

-- | Async with exceptions helpers
module AsyncEx where

import Control.Concurrent
import Control.Exception

data Async a =
  Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action =
  do var <- newEmptyMVar
     _ <- forkIO $ try action >>= putMVar var
     return (Async var)

waitCatch
  :: Async t -> IO (Either SomeException t)
waitCatch (Async var) = readMVar var

wait :: Async b -> IO b
wait action =
  do r <- waitCatch action
     case r of
       Left e -> throwIO e
       Right a -> return a

waitEither
  :: Async a -> Async b -> IO (Either a b)
waitEither a b =
  do m <- newEmptyMVar
     _ <- forkIO $ try (Left <$> wait a) >>= putMVar m
     _ <- forkIO $ try (Right <$> wait b) >>= putMVar m
     wait (Async m)

waitAny :: [Async a] -> IO a
waitAny as =
  do m <- newEmptyMVar
     let forkWait a = forkIO $ try (wait a) >>= putMVar m
     mapM_ forkWait as
     wait (Async m)
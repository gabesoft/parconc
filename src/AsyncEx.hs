-- | Async with exceptions helpers
module AsyncEx where

import Control.Concurrent
import Control.Exception

data Async a =
  Async ThreadId
        (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action =
  do m <- newEmptyMVar
     t <- forkFinally action (putMVar m)
     return (Async t m)

waitCatch
  :: Async t -> IO (Either SomeException t)
waitCatch (Async _ var) = readMVar var

wait :: Async b -> IO b
wait action =
  do r <- waitCatch action
     case r of
       Left e -> throwIO e
       Right a -> return a

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

waitEither
  :: Async a -> Async b -> IO (Either a b)
waitEither a b =
  do m <- newEmptyMVar
     _ <- forkIO $ try (Left <$> wait a) >>= putMVar m
     _ <- forkIO $ try (Right <$> wait b) >>= putMVar m
     t <- myThreadId
     wait (Async t m)

waitAny :: [Async a] -> IO a
waitAny as =
  do m <- newEmptyMVar
     let forkWait a = forkIO $ try (wait a) >>= putMVar m
     mapM_ forkWait as
     t <- myThreadId
     wait (Async t m)

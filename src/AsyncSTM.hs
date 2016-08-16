-- | Async using STM
module AsyncSTM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

data Async a =
  Async ThreadId
        (STM (Either SomeException a))

instance Functor Async where
  fmap f (Async t stm) = Async t ((fmap . fmap) f stm)

async :: IO a -> IO (Async a)
async action =
  do var <- newEmptyTMVarIO
     t <-
       forkFinally action
                   (atomically . putTMVar var)
     return (Async t (readTMVar var))

waitCatchSTM
  :: Async t -> STM (Either SomeException t)
waitCatchSTM (Async _ stm) = stm

waitSTM :: Async b -> STM b
waitSTM action =
  do r <- waitCatchSTM action
     case r of
       Left e -> throwSTM e
       Right a -> return a

wait :: Async a -> IO a
wait = atomically . waitSTM

waitEither
  :: Async a -> Async b -> IO (Either a b)
waitEither a b =
  atomically $ (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth a1 a2 =
  atomically $
  do r1 <- waitSTM a1 `orElse` (waitSTM a2 >> retry)
     r2 <- waitSTM a2
     return (r1,r2)

waitAny :: [Async a] -> IO a
waitAny = atomically . foldr (orElse . waitSTM) retry

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

withAsync :: IO a -> (Async a -> IO c) -> IO c
withAsync io = bracket (async io) cancel

dualAsync
  :: (Async a -> Async b -> IO c) -> IO a -> IO b -> IO c
dualAsync f ioa iob = withAsync ioa $ \a -> withAsync iob $ \b -> f a b

concurrently :: IO a -> IO b -> IO (a,b)
concurrently = dualAsync waitBoth

race :: IO a -> IO b -> IO (Either a b)
race = dualAsync waitEither

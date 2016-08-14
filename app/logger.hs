-- | Logging service
module Main (main) where

import Control.Concurrent

data Logger =
  Logger (MVar LogCommand)

data LogCommand
  = Message String
  | Stop (MVar ())

initLogger :: IO Logger
initLogger =
  do m <- newEmptyMVar
     let l = Logger m
     forkIO $ logger l
     return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where loop =
          do cmd <- takeMVar m
             case cmd of
               Message msg -> putStrLn msg >> loop
               Stop s -> putStrLn "logger: stop" >> putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

worker :: Logger -> IO ThreadId
worker l = forkIO $ do
  threadDelay 10
  logMessage l "starting work"
  threadDelay 10
  logMessage l "still working"
  threadDelay 20
  logMessage l "worker done"

main :: IO ()
main = do
  l <- initLogger
  _ <- worker l
  logMessage l "hello"
  putStrLn "main thread here"
  logMessage l "good bye"
  putStrLn "main thread again"
  logMessage l "so long"
  threadDelay 100
  logStop l
-- | Making http calls in parallel
module Main (main) where

import Control.Concurrent
import Data.ByteString as B
import GetURL

main =
  do m1 <- newEmptyMVar
     m2 <- newEmptyMVar
     forkIO $ getURL "http://www.imdb.com/title/tt4196776/" >>= putMVar m1
     forkIO $ getURL "http://www.imdb.com/title/tt3640424/" >>= putMVar m2
     r1 <- takeMVar m1
     r2 <- takeMVar m2
     print (B.length r1,B.length r2)
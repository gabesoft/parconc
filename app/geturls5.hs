-- | Making http requests in parallel - merging
module Main (main) where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString    as B
import           GetURL
import           Text.Printf

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

main :: IO ()
main =
  do m <- newEmptyMVar
     let download url = getURL url >>= \r -> putMVar m (url,r)
     mapM_ (forkIO . download) sites

     (url,r) <- takeMVar m
     printf "%s was first (%d bytes)\n" url (B.length r)
     replicateM_ (Prelude.length sites - 1) (takeMVar m)

-- | Parallel http requests with cancel on exception
module Main (main) where

import AsyncSTM
import Data.ByteString as B
import Data.List
import GetURL

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.nonexistent1233.com/"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

main :: IO ()
main =
  withAsync (getURL $ sites !! 0) $
  \a0 ->
    withAsync (getURL $ sites !! 1) $
    \a1 ->
      do r0 <- wait a0
         r1 <- wait a1
         print (B.length r0,B.length r1)
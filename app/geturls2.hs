-- | Making http requests in parallel
module Main (main) where

import Async
import Data.ByteString as B
import GetURL

main =
  do a1 <- async (getURL "http://www.imdb.com/title/tt4196776/")
     a2 <- async (getURL "http://www.imdb.com/title/tt3640424/")
     r1 <- wait a1
     r2 <- wait a2
     print (B.length r1,B.length r2)
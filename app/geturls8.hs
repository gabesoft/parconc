-- | Parallel http requests with cancel on exception
module Main (main) where

import AsyncSTM
import Control.Arrow ((***))
import Control.Monad (join)
import Data.ByteString as B
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
  concurrently (getURL $ sites !! 0)
               (getURL $ sites !! 1) >>=
  print . join (***) B.length
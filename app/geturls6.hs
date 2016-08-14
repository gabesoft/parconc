-- | Making http requests in parallel
module Main (main) where

import AsyncEx
import Data.ByteString as B
import GetURL
import Text.Printf

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

main :: IO ()
main =
  do let download url = getURL url >>= \r -> return (url,r)
     as <- mapM (async . download) sites
     (url,r) <- waitAny as
     printf "%s was first (%d bytes)\n" url (B.length r)
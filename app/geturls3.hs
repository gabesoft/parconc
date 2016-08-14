-- | Making http requests in parallel
module Main (main) where

import AsyncEx
import Data.ByteString as B
import GetURL
import Text.Printf
import TimeIt

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.nonexistent1233.com/"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

timeDownload :: String -> IO ()
timeDownload url =
  do printf "downloading %s ...\n" url
     (page,time) <- timeit $ getURL url
     printf "downloaded %s (%d bytes, %.2fs)\n" url (B.length page) time

waitAndLog :: Async a -> IO ()
waitAndLog action =
  do r <- waitCatch action
     case r of
       Left e -> print e
       Right _ -> return ()

main :: IO ()
main =
  do as <- mapM (async . timeDownload) sites
     mapM_ waitAndLog as
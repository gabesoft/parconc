-- | Canceling async downloads
module Main (main) where

import AsyncEx
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Either
import GetURL
import System.IO
import Text.Printf
import TimeIt

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.serpentine.com/wreq/tutorial.html"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

timeDownload :: String -> IO ()
timeDownload url =
  do printf "downloading %s ...\n" url
     (page,time) <- timeit $ getURL url
     printf "downloaded %s (%d bytes, %.2fs)\n" url (B.length page) time

main :: IO ()
main =
  do as <- mapM (async . timeDownload) sites
     _ <-
       forkIO $
       do hSetBuffering stdin NoBuffering
          forever $ getChar >>= \c -> when (c == 'q') $ mapM_ cancel as
     rs <- mapM waitCatch as
     printf "%d/%d succeeded\n"
            (length $ rights rs)
            (length rs)
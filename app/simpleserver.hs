-- | A simple server
module Main (main) where

import System.IO

talk :: Handle -> IO ()
talk h =
  do hSetBuffering h LineBuffering
     loop
  where loop =
          do line <- hGetLine h
             if line == "end"
                then hPutStrLn h "Thank you for using the doubling service"
                else hPrint h (2 * (read line :: Integer)) >> loop

main = undefined
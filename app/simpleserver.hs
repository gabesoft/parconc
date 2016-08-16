-- | A simple server
module Main (main) where

import Control.Concurrent
import Control.Monad
import Network.Socket
import Network (listenOn,PortID(..))
import System.IO
import Text.Printf

talk :: Handle -> IO ()
talk h =
  do hSetBuffering h LineBuffering
     loop
  where loop =
          do line <- hGetLine h
             if line == "end"
                then hPutStrLn h "Thank you for using the doubling service"
                else hPrint h (2 * (read line :: Integer)) >> loop

port :: Int 
port = 44444

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber (fromIntegral port)
  printf "Listening on port %d\n" port
  forever $ do
    (handle,host,port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle) (const $ hClose handle)

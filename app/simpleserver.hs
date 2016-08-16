-- | A simple server
module Main (main) where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import Network (listenOn, PortID(..), accept)
import Network.Socket (withSocketsDo)
import System.IO
import Text.Printf

strip :: String -> String
strip = T.unpack . T.strip . T.pack

talk :: Handle -> IO ()
talk h =
  do hSetBuffering h LineBuffering
     loop
  where loop =
          do line <- hGetLine h
             if strip line == "end"
                then hPutStrLn h "Thank you for using the doubling service"
                else hPrint h (2 * (read line :: Integer)) >> loop

handleRequest :: Handle -> IO ()
handleRequest h = do
  tId <- myThreadId
  putStrLn $ "Handling request on thread " ++ show tId
  talk h

defaultPort :: Int
defaultPort = 44444

main :: IO ()
main =
  withSocketsDo $
  do sock <- listenOn $ PortNumber (fromIntegral defaultPort)
     printf "Listening on port %d\n" defaultPort
     forever $
       do (handle,host,port) <- accept sock
          printf "Accepted connection from %s: %s\n" host (show port)
          forkFinally (handleRequest handle)
                      (const $ hClose handle)

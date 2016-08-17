-- | Simple server with shared state
module Main (main) where

import AsyncSTM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import qualified Data.Text as T
import Network (listenOn, PortID(..), accept)
import Network.Socket (withSocketsDo)
import System.IO
import Text.Printf
import Control.Concurrent.STM.TVar

defaultPort :: Int
defaultPort = 44444

strip :: String -> String
strip = T.unpack . T.strip . T.pack

talk :: Handle -> TVar Integer -> IO ()
talk h factor =
  do hSetBuffering h LineBuffering
     c <- atomically newTChan
     _ <-
       race (server h factor c)
            (receive h c)
     return ()

receive :: Handle -> TChan String -> IO ()
receive h c = forever (hGetLine h >>= atomically . writeTChan c)

server
  :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c =
  do f <- atomically $ readTVar factor
     hPrintf h "Current factor: %d\n" f
     loop f
  where loop f =
          join $
          atomically $
          do f' <- readTVar factor
             if f /= f'
                then return (newfactor f')
                else (command f . strip) <$> readTChan c
        newfactor f = hPrintf h "new factor: %d\n" f >> loop f
        command _ "end" =
          hPutStrLn h "Thank you for using the doubling service"
        command f ('*':s) =
          atomically (writeTVar factor (read s :: Integer)) >> loop f
        command f line = hPrint h (f * (read line :: Integer)) >> loop f

main :: IO a
main =
  withSocketsDo $
  do sock <- listenOn (PortNumber $ fromIntegral defaultPort)
     printf "Listening on port %d\n" defaultPort
     factor <- atomically $ newTVar 2
     forever $
       do (handle,host,port) <- accept sock
          printf "Accepted connection from %s: %s\n" host (show port)
          forkFinally (talk handle factor)
                      (const $ hClose handle)

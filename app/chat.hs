{-# LANGUAGE RecordWildCards #-}

-- | Mini chat server
module Main
  (main)
  where

import           AsyncSTM
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           Network                      (PortID (..), accept, listenOn)
import           Network.Socket               (withSocketsDo)
import           System.IO
import           Text.Printf

type ClientName = String

data Client =
  Client {clientName     :: ClientName
         ,clientHandle   :: Handle
         ,clientKicked   :: TVar (Maybe String)
         ,clientSendChan :: TChan Message}

data Message
  = Notice String
  | Tell ClientName
         String            -- private message
  | Broadcast ClientName
              String       -- public message
  | Command String

data Server =
  Server {clients :: TVar (Map.Map ClientName Client)}

newClient :: ClientName -> Handle -> STM Client
newClient name handle =
  do c <- newTChan
     k <- newTVar Nothing
     return Client {clientName = name
                   ,clientHandle = handle
                   ,clientSendChan = c
                   ,clientKicked = k}

newServer :: IO Server
newServer = newTVarIO Map.empty >>= return . Server

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg =
  do clientMap <- readTVar clients
     mapM_ (flip sendMessage msg)
           (Map.elems clientMap)

defaultPort :: Int
defaultPort = 44444

main :: IO ()
main =
  withSocketsDo $
  do server <- newServer
     sock <- listenOn (PortNumber $ fromIntegral defaultPort)
     printf "Listening on port %d\n" defaultPort
     forever $
       do (handle,host,port) <- accept sock
          printf "Accepted connection from %s %s\n" host (show port)
          forkFinally (talk handle server)
                      (const $ hClose handle)

talk :: Handle -> Server -> IO ()
talk = undefined

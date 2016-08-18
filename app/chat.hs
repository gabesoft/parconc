{-# LANGUAGE RecordWildCards #-}

-- | Mini chat server
module Main (main) where

import AsyncSTM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception (mask, finally)
import Control.Monad
import Control.Monad.STM
import qualified Data.Map as Map
import Network (PortID(..), accept, listenOn)
import Network.Socket (withSocketsDo)
import System.IO
import Text.Printf

type ClientName = String

data Client =
  Client {clientName :: ClientName
         ,clientHandle :: Handle
         ,clientKicked :: TVar (Maybe String)
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
newServer = Server <$> newTVarIO Map.empty

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg =
  do clientMap <- readTVar clients
     mapM_ (`sendMessage` msg) (Map.elems clientMap)

defaultPort :: Int
defaultPort = 44444

checkAddClient
  :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle =
  atomically $
  do clientMap <- readTVar clients
     if Map.member name clientMap
        then return Nothing
        else do client <- newClient name handle
                writeTVar clients $ Map.insert name client clientMap
                broadcast server $ Notice (name ++ " has connected")
                return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name =
  atomically $
  do modifyTVar' clients $ Map.delete name
     broadcast server $ Notice (name ++ " has disconnected")

tell
  :: Server -> Client -> ClientName -> String -> STM ()
tell Server{..} Client{..} name message =
  do clientMap <- readTVar clients
     case Map.lookup name clientMap of
       Nothing -> return ()
       Just targetClient ->
         sendMessage targetClient
                     (Tell clientName message)

kick
  :: Server -> ClientName -> ClientName -> STM ()
kick Server{..} kickedName name =
  do clientMap <- readTVar clients
     case Map.lookup kickedName clientMap of
       Nothing -> return ()
       Just Client{..} -> writeTVar clientKicked (Just name)

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
talk handle server@Server{..} =
  do tId <- myThreadId
     hSetBuffering stdout NoBuffering
     putStrLn ("Serving request on thread " ++ show tId)
     hSetNewlineMode handle universalNewlineMode
     hSetBuffering handle LineBuffering
     readName
  where readName =
          hPutStrLn handle "What is your name?" >> hGetLine handle >>=
          tryAddName
        tryAddName [] = readName
        tryAddName name = addName name
        addName name =
          mask $
          \restore ->
            do ok <- checkAddClient server name handle
               case ok of
                 Nothing ->
                   restore $
                   hPrintf handle "The name %s is in use, please choose another\n" name >>
                   readName
                 Just client ->
                   restore (runClient server client) `finally`
                   removeClient server name

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} =
  void (race serverThread receiveThread)
  where receiveThread =
          forever $
          do msg <- hGetLine clientHandle
             atomically $
               sendMessage client
                           (Command msg)
        serverThread =
          join $
          atomically $
          do k <- readTVar clientKicked
             case k of
               Just reason ->
                 return $
                 hPutStrLn clientHandle ("You have been kicked " ++ reason)
               Nothing ->
                 do msg <- readTChan clientSendChan
                    return $
                      do continue <- handleMessage server client msg
                         when continue serverThread

handleMessage
  :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
    Notice msg -> output ("*** " ++ msg)
    Tell name msg -> output ("*" ++ name ++ "*:" ++ msg)
    Broadcast name msg -> output ("<" ++ name ++ ">:" ++ msg)
    Command msg ->
      case words msg of
        ["/kick",name] ->
          atomically (kick server name clientName) >> return True
        "/tell":who:what ->
          atomically (tell server client who (unwords what)) >> return True
        ["/quit"] -> return False
        ('/':_):_ ->
          hPutStrLn clientHandle ("Unrecognized command: " ++ msg) >>
          return True
        _ ->
          atomically (broadcast server $ Broadcast clientName msg) >>
          return True
  where output s = hPutStrLn clientHandle s >> return True
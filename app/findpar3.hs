{-# LANGUAGE BangPatterns #-}

-- | Find file - parallel with better semaphore
module Main (main) where

import AsyncSTM
import Control.Exception
import Data.List (sort)
import GHC.Conc
import System.Directory
import System.Environment
import System.FilePath.Posix ((</>))
import Data.IORef

newtype NBSem =
  NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newIORef i

tryWaitNBSem :: NBSem -> IO Bool
tryWaitNBSem (NBSem m) = atomicModifyIORef m dec
  where dec 0 = (0,False)
        dec i =
          let !z = i - 1
          in (z,True)

signalNBSem :: NBSem -> IO ()
signalNBSem (NBSem m) =
  atomicModifyIORef m $
  \i ->
    let !z = i + 1
    in (z,())

subfind :: NBSem
        -> String
        -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)]
        -> IO (Maybe FilePath)
subfind sem str path inner asyncs =
  do isDir <- doesDirectoryExist path
     if isDir
        then handleDir
        else inner asyncs
  where handleDir =
          do q <- tryWaitNBSem sem
             if q
                then do let dofind =
                              find sem str path `finally` signalNBSem sem
                        withAsync dofind $ \a -> inner (a : asyncs)
                else do r <- find sem str path
                        maybe (inner asyncs)
                              (const $ return r)
                              r

find
  :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
find sem str dir =
  do contents <- getDirectoryContents dir
     let fs = sort $ filter (`notElem` [".",".."]) contents
     if str `elem` fs
        then return (Just $ dir </> str)
        else foldr (subfind sem str . (dir </>)) dowait fs []
  where dowait
          :: [Async (Maybe FilePath)] -> IO (Maybe FilePath)
        dowait as = loop (reverse as)
        loop
          :: [Async (Maybe FilePath)] -> IO (Maybe FilePath)
        loop [] = return Nothing
        loop (a:as) =
          wait a >>=
          maybe (loop as)
                (return . Just)

main :: IO ()
main =
  do [s,d] <- getArgs
     n <- getNumCapabilities
     sem <-
       newNBSem (if n == 1
                    then 0
                    else n * 4)
     find sem s d >>= print
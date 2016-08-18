{-# LANGUAGE BangPatterns #-}

-- | Find file - parallel with semaphore
module Main (main) where

import AsyncSTM
import Control.Concurrent.MVar
import Control.Exception
import Data.List (sort)
import System.Directory
import System.Environment
import System.FilePath.Posix ((</>))

newtype NBSem =
  NBSem (MVar Int)

newNBSem :: Int -> IO NBSem
newNBSem i = NBSem <$> newMVar i

tryAquireNBSem :: NBSem -> IO Bool
tryAquireNBSem (NBSem m) =
  modifyMVar m
             (return . dec)
  where dec 0 = (0,False)
        dec i =
          let !z = i - 1
          in (z,True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  modifyMVar m $
  \i ->
    let !z = i + 1
    in return (z,())

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
          do q <- tryAquireNBSem sem
             if q
                then do let dofind =
                              find sem str path `finally` releaseNBSem sem
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
  do [n,s,d] <- getArgs
     sem <- newNBSem (read n)
     find sem s d >>= print
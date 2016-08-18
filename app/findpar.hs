-- | Find file - parallel
module Main (main) where

import AsyncSTM
import Data.List (sort)
import System.Directory
import System.Environment
import System.FilePath.Posix ((</>))

subfind :: String
        -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)]
        -> IO (Maybe FilePath)
subfind str path inner asyncs =
  do isDir <- doesDirectoryExist path
     if isDir
        then withAsync (find str path) $ \a -> inner (a : asyncs)
        else inner asyncs

find :: String -> FilePath -> IO (Maybe FilePath)
find str dir =
  do contents <- getDirectoryContents dir
     let fs = sort $ filter (`notElem` [".",".."]) contents
     if str `elem` fs
        then return (Just $ dir </> str)
        else foldr (subfind str . (dir </>)) dowait fs []
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
     find s d >>= print
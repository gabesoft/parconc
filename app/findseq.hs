-- | Find files - sequential
module Main (main) where

import Data.List (sort)
import System.Directory
import System.FilePath.Posix ((</>))
import System.Environment

find :: String -> FilePath -> IO [FilePath]
find searchFile searchDir =
  do contents <- getDirectoryContents searchDir
     let fs = sort $ filter (`notElem` [".",".."]) contents
     if searchFile `elem` fs
        then return [searchDir </> searchFile]
        else loop fs
  where loop [] = return []
        loop (x:xs) =
          do let next = searchDir </> x
             isDir <- doesDirectoryExist next
             if isDir
                then find searchFile next >>= \f -> (f ++) <$> loop xs
                else loop xs

main :: IO ()
main =
  do [s,d] <- getArgs
     find s d >>= mapM_ putStrLn
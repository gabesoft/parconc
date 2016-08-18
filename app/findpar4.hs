{-# LANGUAGE BangPatterns #-}

-- | Find file - parallel with ParIO
module Main (main) where

import AsyncSTM
import Control.Exception
import Control.Monad.IO.Class
import qualified Control.Monad.Par as Par
import Control.Monad.Par.IO as ParIO
import Data.List (sort)
import GHC.Conc
import System.Directory
import System.Environment
import System.FilePath.Posix ((</>))

subfind :: String
        -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        -> [IVar (Maybe FilePath)]
        -> ParIO (Maybe FilePath)
subfind s p inner ivars =
  do isdir <- liftIO $ doesDirectoryExist p
     if not isdir
        then inner ivars
        else do v <- Par.new                   -- 1
                Par.fork (find s p >>= Par.put v)  -- 2
                inner (v : ivars)

-- subfind :: String
--         -> FilePath
--         -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
--         -> [IVar (Maybe FilePath)]
--         -> ParIO (Maybe FilePath)
-- subfind str path inner ivars =
--   do isDir <- liftIO $ doesDirectoryExist path
--      if isDir
--         then handleDir
--         else inner ivars
--   where handleDir =
--           do v <- new
--              fork (find str path >>= put v)
--              inner (v : ivars)
find
  :: String -> FilePath -> ParIO (Maybe FilePath)
find str dir =
  do contents <- liftIO $ getDirectoryContents dir
     let fs = sort $ filter (`notElem` [".",".."]) contents
     if str `elem` fs
        then return (Just $ dir </> str)
        else foldr (subfind str . (dir </>)) dowait fs []
  where dowait
          :: [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
        dowait as = loop (reverse as)
        loop
          :: [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
        loop [] = return Nothing
        loop (a:as) =
          do r <- Par.get a
             maybe (loop as)
                   (return . Just)
                   r

main :: IO ()
main =
  do [s,d] <- getArgs
     ParIO.runParIO (find s d) >>= print
-- | Reminders
module Main (main) where

import Control.Concurrent
import Text.Printf

main :: IO ()
main = loop
  where loop =
          do s <- getLine
             case s of
               "exit" -> return ()
               _ ->
                 do _ <- forkIO (setReminder s)
                    loop

setReminder :: String -> IO ()
setReminder s =
  do let t = read s :: Int
     printf "Ok, I'll remind you in %d seconds\n" t
     threadDelay (10 ^ 6 * t)
     printf "%d seconds is up! BING!\BEL\n" t
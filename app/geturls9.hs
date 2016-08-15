-- | Parallel http requests with cancel on exception
module Main (main) where

import           AsyncSTM
import qualified Data.ByteString as B
import           GetURL

sites :: [String]
sites =
  ["http://www.imdb.com/title/tt4196776/"
  ,"http://www.imdb.com/title/tt3640424/"
  ,"http://www.bing.com/"
  ,"http://www.wired.com/2009/08/ff-craigslist/"]

main :: IO ()
main =
  foldr conc
        (return [])
        (getURL <$> sites) >>=
  print . (fmap B.length)
  where conc ioa ioas = uncurry (:) <$> concurrently ioa ioas

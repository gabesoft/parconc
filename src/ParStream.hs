{-# LANGUAGE BangPatterns #-}

-- | Pipeline parallelism
module ParStream where

import Control.Monad
import Control.Monad.Par.Scheds.Trace
import Control.DeepSeq

data IList a
  = Nil
  | Cons a
         (IVar (IList a))
  deriving ((((((((Eq))))))))

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

type Stream a = IVar (IList a)

streamFromList :: NFData a
               => [a] -> Par (Stream a)
streamFromList xs =
  do var <- new
     fork $ loop xs var
     return var
  where loop [] var = put var Nil
        loop (y:ys) var =
          do rest <- new
             put var (Cons y rest)
             loop ys rest

streamFold
  :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc inStream =
  do iList <- get inStream
     case iList of
       Nil -> return acc
       Cons h t ->
         streamFold fn
                    (fn acc h)
                    t

streamMap
  :: NFData b
  => (a -> b) -> Stream a -> Par (Stream b)
streamMap f inStream =
  do outStream <- new
     fork $ loop inStream outStream
     return outStream
  where loop xs ys =
          do iList <- get xs
             case iList of
               Nil -> put ys Nil
               Cons h t ->
                 do rest <- new
                    put ys (Cons (f h) rest)
                    loop t rest

sample :: Integer
sample =
  runPar $
  join $
  streamFold (+) 0 <$>
  join (streamMap (+ 1) <$> streamFromList [1 .. 10 :: Integer])
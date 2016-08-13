{-# LANGUAGE CPP, BangPatterns, TypeOperators #-}

-- | Repa examples
module RepaEx where

import Data.Array.Repa as R
import Data.Vector.Unboxed.Base

unboxedD1 :: Unbox a
          => [a] -> Array U DIM1 a
unboxedD1 xs =
  fromListUnboxed (Z :. length xs)
                  xs

-- unboxedD2 [[1..4],[2..5],[5..8],[6..9],[3..6]]
unboxedD2 :: Unbox a
          => [[a]] -> Array U DIM2 a
unboxedD2 [] =
  fromListUnboxed (Z :. 0 :. 0)
                  []
unboxedD2 xss =
  let n = length xss
      m = length $ head xss
  in fromListUnboxed (Z :. n :. m)
                     (concat xss)

-- delayedD1 10 (\(Z:.i) -> i :: Int)
delayedD1
  :: head -> (Z :. head -> a) -> Array D (Z :. head) a
delayedD1 n = fromFunction (Z :. n)

getD1 :: Unbox a
      => Array U DIM1 a -> Int -> a
getD1 xs i = xs ! (Z :. i)

getD2 :: Unbox a
      => Array U DIM2 a -> Int -> Int -> a
getD2 xs r c = xs ! (Z :. r :. c)

mapAndCompute
  :: (Shape sh,Source r a,Unbox e)
  => (a -> e) -> Array r sh a -> Array U sh e
mapAndCompute f xs = computeUnboxedS $ R.map f xs

-- computeUnboxedS $ rmap (+ 1) (unboxedD1 [1 .. 10 :: Int])
rmap :: (Shape sh,Source r e)
     => (e -> a) -> Array r sh e -> Array D sh a
rmap f a =
  fromFunction (extent a)
               (\i -> f (a ! i))
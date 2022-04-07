{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module L5 where

import Data.Array

converge :: (Ord a, Num a) => a -> [a] -> a
converge eps (m:n:rest)
    | abs (m - n) < eps = m
    | otherwise         = converge eps (n:rest)

easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = (f (x + h) - f x) / h

halves :: Fractional a => a -> [a]
halves = iterate (/2)

diff :: (Ord a, Fractional a) => a -> a -> (a -> a) -> a -> a
diff h0 eps f x = converge eps $ map (easydiff f x) $ halves h0

fib :: Integer -> Integer
fib n | n <= 1 = n
      | otherwise = fib (n-1) + fib (n-2)

fib2 :: Integer -> Integer
fib2 n = fibs !! fromIntegral n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' op acc (x:xs) = let t = op acc x
                            in t `seq` foldl' op t xs


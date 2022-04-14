module Sorting where

import Control.Parallel (par, pseq)
import qualified Data.List as L 

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = lesser ++ x:greater
    where
        lesser  = sort [y | y <- xs, y < x]
        greater = sort [y | y <- xs, y >= x]

seqSort :: Ord a => [a] -> [a]
seqSort [] = []
seqSort (x:xs) = lesser `pseq` (greater `pseq` (lesser ++ x:greater))
    where
        lesser  = seqSort [y | y <- xs, y <  x]
        greater = seqSort [y | y <- xs, y >= x]

parSort2 :: Ord a => Int -> [a] -> [a]
parSort2 d list@(x:xs)
    | d <= 1    = L.sort list 
    | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
    where
        lesser  = parSort2 d' [y | y <- xs, y <  x]
        greater = parSort2 d' [y | y <- xs, y >= x]
        d'      = d - 1
parSort2 _ _ = []

force :: [a] -> ()
force xs = go xs `pseq` ()
    where
        go (_:xs) = go xs
        go [] = 1
{-
main = do
    list <- (map read.words) <$> getLine
    let sortedList = parSort list :: [Int]
    putStrLn . show $ sortedList
-}

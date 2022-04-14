module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import Sorting
import System.Random (StdGen, getStdGen, randoms)
import qualified Data.List as L
import GHC.IO.Handle (NewlineMode(inputNL))
import GHC.Conc (numCapabilities)

randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g)
    in force result `seq` result

testFunction1 = parSort2
--testFunction2 = seqSort
--seqSort = Sorting.sort

main :: IO ()
main = do
    args <- getArgs 
    let count | null args = 50000
              | otherwise = read (head args)
    input <- randomInts count <$> getStdGen
    --putStrLn $ "We have input " ++ show input
    start <- getCurrentTime
    let sorted = testFunction1 numCapabilities input
    putStrLn $ "Sorted: " ++ show (length sorted)
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed"
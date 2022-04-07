module Main where
import Data.List (foldl')

sum' = foldl' (+) 0

main = print $ sum' [0.1,0.2..1e5]


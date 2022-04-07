--{-# LANGUAGE FlexibleContexts #-}
-- module Mutable (
--     Mutable -- Mem, purge,
--     --read, write, new
--     ) where

-- -- import State

-- -- newtype Mutable s a = Mutable (State s a)
-- -- data FakeState = FakeState

-- -- purge :: Mutable s a -> a
-- -- purge (Mutable a) = evalState $ runState a FakeState

-- -- new :: a -> Mutable s a
-- -- read :: Mem s a -> Mutable s a
-- -- write :: Mem s a -> a -> Mutable s ()

-- --let mem = purge allocate in purge (read mem)

import Data.STRef
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Control.Monad (when)

swapElems :: Ix i => i -> i -> STArray s i e -> ST s ()
swapElems i j arr = do
    vi <- readArray arr i
    vj <- readArray arr j

    writeArray arr i vj
    writeArray arr j vi


test :: Int -> Int -> [a] -> [a]
test i j xs = elems $ runSTArray $ do
    arr <- newListArray (0, length xs -1) xs
    swapElems i j arr
    return arr

qsort :: Ord a => [a] -> [a]
qsort xs = elems $ runSTArray $ do
    arr <- newListArray (left, right) xs
    qsortST left right arr
    return arr
    where
        left = 0
        right = length xs - 1

qsortST :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsortST left right arr = do
    when (left  <= right) $ do
        swapElems left (div (left + right) 2) arr
        vleft <- readArray arr left
        (last, _) <- forloop (left + 1) (<= right) succ
            (update vleft) (return (left, arr))
        swapElems left last arr
        qsortST left (last - 1) arr
        qsortST (last + 1) right arr


update :: (Ix a, Enum a, Ord e) => e -> a -> ST s (a, STArray s a e) -> ST s (a, STArray s a e)
update vLeft i st = do
    (last, arr) <- st
    vi <- readArray arr i
    if vi < vLeft
        then do
            swapElems (succ last) i arr
            return (succ last, arr)
        else do
            return (last, arr)

forloop :: i -> (i -> Bool) -> (i -> i) -> (i -> s -> s) -> s -> s
forloop i0 pred next update s0 = runST $ do
    refI <- newSTRef i0
    refS <- newSTRef s0
    iter refI refS
    readSTRef refS
    where
        iter refI refS = do
            i <- readSTRef refI
            s <- readSTRef refS
            when (pred i) $ do
                writeSTRef refI $ next i
                writeSTRef refS $ update i s
                iter refI refS

module Main where

import Control.Concurrent.STM
import Control.Monad (replicateM_)

makeCounter :: IO (IO Int)
makeCounter = do
    var <- newTVarIO 1
    return $ atomically $ do
        val <- readTVar var
        writeTVar var (val+1)
        return val

main :: IO ()
main = do
    counter <- makeCounter
    replicateM_ 10 $ counter >>= print

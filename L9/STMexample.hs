import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

example :: IO Integer
example = do
    account1 <- atomically $ newTVar 5000
    account2 <- atomically $ newTVar 1000
    atomically $ transfer 500 account1 account2
    readTVarIO account1

-- atomically :: STM a -> IO a
-- retry :: STM a

transfer :: Integer -> TVar Integer -> TVar Integer -> STM ()
transfer n from to = do
    modifyTVar from (+ (-n))
    modifyTVar to (+ n)

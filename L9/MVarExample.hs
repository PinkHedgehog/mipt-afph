import Control.Concurrent
import Control.Monad (forever)


communicate :: IO ()
communicate = do
    m <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("received " ++ show v)
    putStrLn "sending"
    putMVar m "wake up!"

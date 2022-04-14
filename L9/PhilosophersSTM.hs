module PhilosophersSTM where

import GHC.Conc (TVar, STM, readTVar, writeTVar, retry, atomically, threadDelay, newTVarIO, forkIO)
import Control.Monad (when, unless)
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering, BlockBuffering))
import System.IO (stdout)

data ForkState = Free | Taken deriving (Show, Eq, Read)
type TFork = TVar ForkState

data Forks = Forks
           { fork1 :: TFork
           , fork2 :: TFork
           , fork3 :: TFork
           , fork4 :: TFork
           , fork5 :: TFork
           }

takeFork :: TFork -> STM Bool
takeFork tFork = do
    forkState <- readTVar tFork
    when (forkState == Free) $ writeTVar tFork Taken
    pure (forkState == Free)

-- takeForks :: (TFork, TFork) -> STM Bool
-- takeForks (tLeftFork, tRightFork) = do
--     leftTaken <- takeFork tLeftFork
--     rightTaken <- takeFork tRightFork
--     pure $ leftTaken && rightTaken
takeForks :: (TFork, TFork) -> STM Bool
takeForks (tLeftFork, tRightFork) = do
    leftTaken <- takeFork tLeftFork
    rightTaken <- takeFork tRightFork
    --when (not leftTaken || not rightTaken) retry
    pure $ leftTaken && rightTaken


data PhilosopherState = Thinking | Eating deriving (Show, Eq, Read)

data Philosopher = Philosopher
                 { pName       :: String
                 , pState      :: TVar PhilosopherState
                 , pLeftFork   :: TFork
                 , pRrightFork :: TFork
                 }


changePhilosopherActivity :: Philosopher -> STM (String, PhilosopherState)
changePhilosopherActivity (Philosopher name tState tLeftFork tRightFork) = do
    state <- readTVar tState
    --name <- readTVar tName
    case state of
        Thinking -> do
            taken <- takeForks (tLeftFork, tRightFork)
            unless taken retry
            writeTVar tState Eating
            pure (name, Eating)
        Eating -> do
            writeTVar tLeftFork Free
            writeTVar tRightFork Free
            writeTVar tState Thinking
            pure (name, Thinking)

philosoperWorker :: Philosopher -> IO ()
philosoperWorker philosopher = do
    print =<< atomically (changePhilosopherActivity philosopher)
    threadDelay 1000000
    philosoperWorker philosopher

runPhilosophers :: IO ()
runPhilosophers = do
    hSetBuffering stdout (BlockBuffering Nothing)
    tState1 <- newTVarIO Thinking
    tState2 <- newTVarIO Thinking
    tState3 <- newTVarIO Thinking
    tState4 <- newTVarIO Thinking
    tState5 <- newTVarIO Thinking
    tFork1  <- newTVarIO Free
    tFork2  <- newTVarIO Free
    tFork3  <- newTVarIO Free
    tFork4  <- newTVarIO Free
    tFork5  <- newTVarIO Free

    forkIO (philosoperWorker (Philosopher "1" tState1 tFork1 tFork2))
    forkIO (philosoperWorker (Philosopher "2" tState2 tFork2 tFork3))
    forkIO (philosoperWorker (Philosopher "3" tState3 tFork3 tFork4))
    forkIO (philosoperWorker (Philosopher "4" tState4 tFork4 tFork5))
    forkIO (philosoperWorker (Philosopher "5" tState5 tFork5 tFork1))

    threadDelay 13000000
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
    return :: a -> State s a
    return x = State (x,)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) ma mf = State $ \s -> 
        let (a1, s1) = runState ma s
            (a2, s2) = runState (mf a1) s1
            in (a2, s2)

instance Functor (State s) where
    -- if Applicative or Monad is implemented:
    fmap :: (a -> b) -> State s a -> State s b
    fmap = (<*>) . pure -- or fmap f mx = return . f =<< mx
                        -- or fmap f = (=<<) (return . f)

    -- if Monad and Applicative aren't implemented
    -- fmap f mx = State $ \s ->
    --     let (x, s') = runState mx s
    --         in (f x, s')
 



instance Applicative (State s) where
    pure = return
    -- if Functor and Monad are already implemented:
    -- (<*>) mf mx = do
    --     f <- mf
    --     f <$> mx

    -- if not:
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) mf mx = State $ \s ->
        let (f, s1) = runState mf s
            (x, s2) = runState mx s1
            in (f x, s2)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put $ f s

-- get >>= \s -> put (f s)
test :: State Int Int
test = do
    put 3
    modify (+ 4)
    get

execState :: State s a -> s -> s
execState act = snd . runState act

evalState :: State s a -> s -> a
evalState act = fst . runState act

data FSM = FSM
         { states          :: [Int]
         , transitions     :: Int -> Char -> Maybe Int
         , initState       :: Int
         , acceptingStates :: [Int] 
         }

traverse :: String -> FSM -> (Int, Bool)
traverse str fsm = (finalState, isAcc)
    where
        finalState = evalState (traverse' str) (initState fsm)
        isAcc = elem finalState $ acceptingStates fsm
        traverse' :: String -> State Int Int
        traverse' "" = get
        traverse' (x:xs) = do
            curState <- get
            case transitions fsm curState x of
                Just newState -> do
                    put newState
                    traverse' xs
                Nothing -> get

sampleTransitions :: Int -> Char -> Maybe Int
sampleTransitions 0 '0' = Just 1
sampleTransitions 1 '1' = Just 0
sampleTransitions 1 '0' = Just 2
sampleTransitions _  _  = Nothing

sampleFSM = FSM 
          { initState       = 0
          , transitions     = sampleTransitions
          , states          = [0, 1, 2]
          , acceptingStates = [2]
          }
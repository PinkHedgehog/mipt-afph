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
    

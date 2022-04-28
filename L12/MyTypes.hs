{-# LANGUAGE UndecidableInstances #-}
module MyTypes where

data Unit = MkUnit

data HigherKinded f a = Bare a | Wrapped (f a)

instance Monad m => Monad (HigherKinded m) where
    return = Bare
    (Bare x) >>= y = y x
    -- HigherKinded IO a -> (a -> HigherKinded IO a) -> HigherKinded IO a
    (Wrapped mx) >>= y = mx >>= y

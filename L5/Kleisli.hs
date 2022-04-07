{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Kleisli where

import Prelude hiding (id, (>>), (*>), sequence)

class Category cat where
    id :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idK :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

instance Category (->) where
    id = \x -> x
    (>>) = flip (.)


instance Kleisli Maybe where
    idK = Just
    f *> g = maybe Nothing g . f

impureDiv :: Int -> Int -> Maybe Int
impureDiv _ 0 = Nothing
impureDiv x y = Just $! div x y

instance Kleisli [] where
    idK = \x -> [x]
    f *> g = f >> map g >> concat

next :: Char -> String
next 'a' = "ab"
next 'b' = "a"
next _   = ""

-- generate :: Int -> (a -> [a]) -> (a -> [a])
-- generate n f = iterate (*> f) idK !! n

(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()

(+$) :: Kleisli m => (a -> b) -> m a -> m b
f +$ a = (const a +> f) ()

($$) :: Kleisli m => m (a -> b) -> m a -> m b
mf $$ ma = (+$ ma) *$ mf

lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
lift2 f a b = (f +$ a) $$ b

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) $ idK []

-- to do
instance (Kleisli m, Applicative m) => Monad m where
    return = undefined
    (>>=) = undefined

instance Monad m => Kleisli m where
    idK = undefined
    (*>) = undefined
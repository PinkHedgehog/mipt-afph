{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, UndecidableInstances #-}

--data Bool = True | False
data Unit = MkUnit
data IntAndChar = MkIntChar Int Char

data HigherKinded f a
    = Bare a
    | Wrapped (f a)

-- data Zero
-- data Succ a

-- type One = Succ Zero
-- type Two = Succ One
-- type Three = Succ Two

data Nat = Zero | Succ Nat

-- data Maybe a where
--     Just :: a -> Maybe a
--     Nothing :: Maybe a

data IntBool a where
    Int :: Int -> IntBool Int
    Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a
extractIntBool (Int _) = 0
extractIntBool (Bool b) = b

data Vector (n :: Nat) (a :: *) where
    VNil :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
    show VNil = "VNil"
    show (VCons x xs) = "VCons " ++ show x ++ " (" ++ show xs ++ ")"


type family Add n m where
    Add 'Zero n     = n
    Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = rest
append (VCons a rest) xs = VCons a (append rest xs)

safeTail :: Vector ('Succ n) a -> Vector n a
safeTail (VCons _ rest) = rest 


module List where

-- define our own linked list
data List a = Null | Cons a (List a) deriving (Eq, Show)

-- define concatenation operator for our list
(.++.) :: List a -> List a -> List a
(.++.) Null x = x
(.++.) x Null = x
(Cons x xs) .++. rest = Cons x (xs .++. rest)

-- if type 'a' is instance of Ord typeclass,
-- then 'List a' is sortable
sort :: Ord a => List a -> List a
sort Null = Null
sort (Cons x xs) = sort lower .++. Cons x Null .++. sort greater
    where
        filter' :: (a -> Bool) -> List a -> List a
        filter' _ Null = Null
        filter' p (Cons x xs) = if p x then Cons x (filter' p xs)
                                       else filter' p xs

        lower = filter' (< x) xs
        greater = filter' (>= x) xs

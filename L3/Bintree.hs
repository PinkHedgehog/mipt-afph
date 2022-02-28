data Bintree a = Null | Node a (Bintree a) (Bintree a)

insert :: Ord a => a -> Bintree a -> Bintree a
insert x Null = Node x Null Null
insert x (Node y left right) = error "Not implemented yet"

instance Show a => Show (Bintree a) where
    show tree = error "Not implemented yet"

-- How can one test implementation?
-- In interactive session run sth like
-- insert a $ insert b $ insert c ... $ Null
-- where 'a', 'b', 'c' are Ord values of the same type
-- :i in ghci shows types, which are instances of Ord 



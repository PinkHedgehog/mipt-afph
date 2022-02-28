module Stack where


-- let's define Stack data structure!
class Stack s where -- 's' is an abstraction of stack
                    -- it can be list, hashmap, or sth else
                    -- 'a' is a type of elements
    empty   :: s a               -- empty stack
    pop     :: s a -> s a        -- pop element from top of the stack
    peek    :: s a -> a          -- show element on top of the stack 
    push    :: a   -> s a -> s a -- insert element into the stack
    isEmpty :: s a -> Bool       -- check if the stack is empty

-- implementation of stack based on the list from Prelude/Data.List
newtype StackList a = StackList [a]
-- 'newtype' is more efficient way to declare new types than 'data',
-- but can be used only when there's just one constructor

instance Stack StackList where
    empty                      = StackList []
    peek    (StackList [])     = error "Empty stack"
    peek    (StackList xs)     = head xs 
    push x  (StackList xs)     = StackList $ x : xs
    pop     (StackList [])     = error "Empty stack"
    pop     (StackList (_:xs)) = StackList xs
    isEmpty (StackList xs)     = null xs

-- functions that have similar pattern
-- and can be generalized
inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

-- both functions above can be generalized using map
-- (the function name below is edited to avoid the built-in on)
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- by using map', inc and sqr can be modified into
inc' = map' (+1)
sqr' = map' (^2)

-- Functor typeclass definition
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Make list into a Functor
-- instance Functor [] where
--   -- fmap :: (a -> b) -> [a] -> [b]
--   fmap = map'

-- Define a Functor for a built in type: Maybe
-- (the deriving below is just for displaying the result)
data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
    -- fmap :: (a -> b) ->  Maybe' a -> Maybe' b
    fmap _ Nothing' = Nothing'
    fmap g (Just' x) = Just' (g x)

-- Create a user-defined type and make it into Functor
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x)   = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)


-- IO is not a container type, but we still can create
-- a Functor instance for IO
-- (there is already one, so it is commented)
-- instance Functor IO where
--    -- fmap :: (a -> b) -> IO a -> IO b
--    fmap g mx = do {x <- mx; return (g x)}

-- We can define the inc and sqr functions above to be functorial
inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)

sqr'' :: Functor f => f Int -> f Int
sqr'' = fmap (^2)

-- Functor laws
--
-- 1.  fmap id      = id
--     id: (a -> a)    id: (f a -> f a)
--
-- 2.  fmap (g . h) = fmap g . fmap h

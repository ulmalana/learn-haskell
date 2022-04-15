-- Applicative
-- generalising functors to take multiple arguments, instead of only one.

-- instead of creating multiple fmap for different number of arguments
-- like below
--
-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) f a -> f b -> f c -> f d
-- .
-- .
-- .
-- we can create this functor into "applicative functor" that takes multiple 
-- arguments with the following 
--
-- class Functor f => Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b

-- example: applying applicative for Maybe
data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap g (Just' x) = Just' (g x)

instance Applicative Maybe' where
    -- pure :: a -> Maybe' a
    pure = Just'

    -- (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    Nothing' <*> _ = Nothing'
    (Just' g) <*> mx = fmap g mx

-- multiplying two lists of integers
-- with list comprehension:
-- 
-- prods :: [Int] -> [Int] -> [Int]
-- prods xs ys = [x*y | x <- xs, y <- ys]
--
-- with applicative:
prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

-- make IO into applicative functor (defined in Prelude)
-- instance Applicative IO where
--    -- pure :: a -> IO a
--    pure = return
--
--    -- (<*>) :: IO (a -> b) -> IO a -> IO b
--    mg <*> mx = do { g <- mg; x <- mx; return (g x)}

-- get n chars from user with applicative
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

-- after all the examples above, there are two ways of looking at applicative
-- functor:
-- 1. generalising the idea of mapping to functions with multiple arguments
-- 2. supporting programming with "effects"
--    abstracting the idea of applying pure function to effectful arguments.
--    arguments are no longer just plain values, but may also have effects,
--    such as possibility of failure, performing IO, etc.

-- executing a list of applicatives
sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = pure (:) <*> x <*> sequenceA' xs

-- redefine getChars with applicative
getChars' :: Int -> IO String
getChars' n = sequenceA' (replicate n getChar)

-- Applicative laws
-- 1. pure id <*> x     = x
-- 2. pure (g x)        = pure g <*> pure x
-- 3. x <*> pure y      = pure (\g -> g y) <*> x
-- 4. x <*> (y <*> z)   = (pure (.) <*> x <*> y) <*> z

-- applicative forms
-- pure g <*> x1 <*> x2 <*> ... <*> xn
-- or
-- g <$> x1 <*> x2 <*> ... <xn>  (used more in practice)
-- (g <$> x = fmap g x)

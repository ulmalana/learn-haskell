module Traversable14 where

import Foldable14
import Monoid14

-- Traversable generalises the idea of mapping a function not only 
-- over a list but also over parameterised types
-- ex: a function that is applied to each element that may fail
traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' g [] = pure []
traverse' g (x:xs) = pure (:) <*> g x <*> traverse' g xs

-- function to decrement a value in Maybe
dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

-- Traversable class declaration
-- 
-- class (Functor t, Foldable t) => Traversable t where
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- Make list into Traversable (already included)
-- instance Traversable [] where
--    -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
--    traverse g [] = pure []
--    traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs

-- Make Tree into Functor to support Traversable
instance Functor Tree where
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- Make Tree into Traversable
instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x) = pure Leaf <*> g x
    traverse g (Node l r) = 
        pure Node <*> traverse g l <*> traverse g r

-- Other primitive function in Traversable class: sequenceA
--
-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id
--
-- sequenceA transforms a data structure whose elements are applicative
-- actions into a single such action that returns a data structure.
-- ex: transform a data structure whose elements may fail into
--     a data structure that may fail
--
--     > sequenceA [Just 1, Just 2, Just 3]
--     Just [1,2,3]
--     > sequenceA [Just 1, Nothing, Just 3]
--     Nothing
--     > sequenceA (Node (Leaf (Just 1)) (Leaf (Just 2)))
--     Just (Node (Leaf 1) (Leaf 2))

-- Default definition for traverse in terms of sequenceA
-- (apply the function first, then combine)
--
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse g = sequenceA . fmap g

-- To declare an instance of Traversable, defining traverse function is enough

-- Two traversable primitives for Monad
--
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
-- mapM = traverse
--
-- sequence :: Monad m => t (m a) -> m (t a)
-- sequence = sequenceA

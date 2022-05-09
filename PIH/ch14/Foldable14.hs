module Foldable14 where

import Monoid14

-- Primary application of monoids: combine all the values in
-- a data structure to give a single value
-- ex: fold function using Monoid
--
--fold' :: Monoid' a => [a] -> a
--fold' [] = mempty'
--fold' (x:xs) = x `mappend'` fold' xs

-- fold for binary trees
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

--fold'' :: Monoid' a => Tree a -> a
--fold'' (Leaf x) = x
--fold'' (Node l r) = fold'' l `mappend'` fold'' r 

-- Foldable: generalising the idea of folding up values in data structure
-- using a monoid in a range of parameterised types,
-- not specific types like list and binary trees

-- foldable class in Data.Foldable
class Foldable' t where
    fold' :: Monoid' a => t a -> a
    foldMap' :: Monoid' b => (a -> b) -> t a -> b
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl' :: (a -> b -> a) -> a -> t b -> a

-- make list into Foldable
instance Foldable' [] where
    -- fold' :: Monoid' a => [a] -> a
    fold' [] = mempty'
    fold' (x:xs) = x `mappend'` fold' xs

    -- foldMap' :: Monoid' b => (a -> b) -> [a] -> b
    foldMap' _ [] = mempty'
    foldMap' f (x:xs) = f x `mappend'` foldMap' f xs

    -- foldr' :: (a -> b -> b) -> b -> [a] -> b
    foldr' _ v [] = v
    foldr' f v (x:xs) = f x (foldr' f v xs)

    -- foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' _ v [] = v
    foldl' f v (x:xs) = foldl' f (f v x) xs

-- Foldable instance for binary tree
instance Foldable' Tree where
    -- fold' :: Monoid' a => Tree a -> a
    fold' (Leaf x) = x
    fold' (Node l r) = fold' l `mappend'` fold' r

    -- foldMap' :: Monoid' b => (a -> b) -> Tree a -> b
    foldMap' f (Leaf x) = f x
    foldMap' f (Node l r) = foldMap' f l `mappend'` foldMap' f r

    -- foldr' :: (a -> b -> b) -> b -> Tree a -> b
    foldr' f v (Leaf x) = f x v
    foldr' f v (Node l r) = foldr' f (foldr' f v r) l

    -- foldl' :: (a -> b -> a) -> a -> Tree b -> a
    foldl' f v (Leaf x) = f v x
    foldl' f v (Node l r) = foldl' f (foldl' f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

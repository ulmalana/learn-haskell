module Monoid14 where

-- monoid : a set with
--            - associative operator that combines 2 elements
--            - identity element for the operator
-- ex: integers set forms a monoid with:
--      - (+) addition operator
--      - 0 as the identity element

-- declare our own class
class Monoid' a where 
    mempty' :: a
    mappend' :: a -> a -> a

    -- example: mconcat for [x,y,z]
    --          x `mappend'` (y `mappend'` (z `mappend'` mempty'))
    mconcat' :: [a] -> a
    mconcat' = foldr mappend' mempty'

-- Monoid needs to follow the following laws
-- 
-- mempty' `mappend'` x = x
-- x `mappend'` mempty' = x
-- x `mappend'` (y `mappend'` z) = (x `mappend'` y) `mappend'` z

-- create an instance of Monoid
instance Monoid' [a] where
    -- mempty' :: [a]
    mempty' = []

    -- mappend' :: [a] -> [a] -> [a]
    mappend' = (++)

-- Monoid for Maybe
instance Monoid' a => Monoid' (Maybe a) where
    -- mempty' :: Maybe a
    mempty' = Nothing

    -- mappend' :: Maybe a -> Maybe a -> Maybe a
    Nothing `mappend'` my = my
    mx `mappend'` Nothing = mx
    Just x `mappend'` Just y = Just (x `mappend'` y)

-- Monoid from Integer with addition
instance Monoid' Int where
    -- mempty' :: Int
    mempty' = 0

    -- mappend' :: Int -> Int -> Int
    mappend' = (+)

-- Another Monoid from Integer with multiplication
-- This will fail since multiple instance declaration of 
-- the same type for the same class are not permitted.
-- (Two instances of Monoid Int are not allowed)
--
-- instance Monoid' Int where
--    -- mempty' :: Int
--    mempty' = 1
--
--    -- mappend' :: Int -> Int -> Int
--    mappend' = (*)

-- To create multiple instances of the same type, use wrapped
-- types for each instance.

-- example: 
-- Monoid for addition (included in Data.Monoid)
newtype Sum' a = Sum' a deriving (Eq, Ord, Show, Read)

getSum' :: Sum' a -> a
getSum' (Sum' x) = x

instance Num a => Monoid' (Sum' a) where
    -- mempty' :: Sum' a
    mempty' = Sum' 0

    -- mappend' :: Sum' a -> Sum' a -> Sum' a
    Sum' x `mappend'` Sum' y = Sum' (x+y)

-- Monoid for multiplication (also included in Data.Monoid)
newtype Product' a = Product' a  deriving (Eq, Ord, Show, Read)

getProduct' :: Product' a -> a
getProduct' (Product' x) = x

instance Num a => Monoid' (Product' a) where
    -- mempty' :: Product' a
    mempty' = Product' 1

    -- mappend' :: Product' a -> Product' a -> Product' a
    Product' x `mappend'` Product' y = Product' (x*y)

-- Shorter version of mappend = <> (included in Data.Monoid)
-- x <> y = x `mappend` y
-- x <> y <> z = x `mappend` y `mappend` z

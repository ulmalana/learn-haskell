-- for functor2 on either int
{-# LANGUAGE FlexibleInstances #-}

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)

-- tree map: change Tree a to Tree (length a)
treeLn (Leaf s) = Leaf (length s)
treeLn (Node l r) = Node (treeLn l) (treeLn r)

-- more general tree map
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

class Functor2 f where
    fmap2 :: (a -> b) -> f a -> f b

instance Functor2 Tree where
    fmap2 = treeMap

instance Functor2 [] where
    fmap2 = map

instance Functor2 Maybe where
    fmap2 _ Nothing = Nothing
    fmap2 f (Just x) = Just (f x)

instance Functor2 (Either Int) where
    fmap2 _ (Left n) = Left n 
    fmap2 f (Right r) = Right (f r)
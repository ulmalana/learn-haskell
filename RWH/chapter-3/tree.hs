data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- exercise
-- maybe tree
data MaybeTree a = Node2 a (Maybe (MaybeTree a)) (Maybe (MaybeTree a)) deriving (Show)
maybeTree = Node2 1 (Just (Node2 2 Nothing Nothing)) (Just (Node2 3 Nothing Nothing))
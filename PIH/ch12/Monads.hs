import Data.Char

data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n -> case eval' y of
                        Nothing -> Nothing
                        Just m -> safediv n m 

-- eval with applicative (not type correct)
-- eval :: Expr -> Maybe Int
-- eval (Val n) = pure n
-- eval (Div x y) = pure safediv <*> eval x <*> eval y

-- eval with monads
eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m  

-- eval with do notation
eval''' :: Expr -> Maybe Int
eval''' (Val n) = Just n
eval''' (Div x y) = do
                    n <- eval''' x
                    m <- eval''' y
                    safediv n m

-- example: applying Monad for Maybe
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

instance Monad Maybe' where
    -- (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    Nothing' >>= _ = Nothing'
    (Just' x) >>= f = f x  


-- Monad for list (already defined in Prelud)
-- instance Monad [] where
--    -- (>>=) :: [a] -> (a -> [b]) -> [b]
--    xs >>= f = [y | x <- xs, y <- f x]

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do 
                x <- xs
                y <- ys
                return (x,y)


-- State Monad
type State = Int
type ST1 = State -> State
type ST2 a = State -> (a, State)

newtype ST3 a = S (State -> (a, State))

app :: ST3 a -> State -> (a, State)
app (S st) x = st x

-- make ST3 into a functor
instance Functor ST3 where
    -- fmap :: (a -> b) -> ST3 a -> ST3 b
    fmap g st = S (\s ->
                  let (x, s') = app st s 
                  in (g x, s'))

-- make ST3 into applicative
instance Applicative ST3 where
    -- pure :: a -> ST3 a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST3 (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
                    let (f, s') = app stf s
                        (x, s'') = app stx s'
                    in (f x, s''))

-- make ST3 into monad
instance Monad ST3 where
    -- (>>=) :: ST3 a -> (a -> ST3 b) -> ST3 b
    st >>= f = S (\s ->
                let (x, s') = app st s 
                in app (f x) s')

-- Relabelling trees
data Tree a = Leaf a | Node (Tree a) (Tree a)
                deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- raw labeling function
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                        where
                            (l', n') = rlabel l n
                            (r', n'') = rlabel r n'

-- return current integere and fresh integer
fresh :: ST3 Int
fresh = S (\n -> (n, n+1))

-- applicative labeling function
alabel :: Tree a -> ST3 (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- monadic labeling function
mlabel :: Tree a -> ST3 (Tree Int)
mlabel (Leaf _) = do
                    n <- fresh
                    return (Leaf n)
mlabel (Node l r) = do
                    l' <- mlabel l
                    r' <- mlabel r
                    return (Node l' r')

-- Generic functions
-- map for monad: mapM (already defined)
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
                    y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

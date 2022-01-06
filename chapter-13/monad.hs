applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x


-- class Monad m where
--     return :: a -> m a 
    
--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b 
--     x >> y = x >>= \_ -> y 

--     fail :: String -> m a 
--     fail msg = error msg

-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     Just x >>= f = f x 
--     fail _ = Nothing

type Birds = Int 
type Pole = (Birds, Birds)

-- landLeft :: Birds -> Pole -> Pole
-- landLeft n (left, right) = (left + n, right)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
 | abs ((left + n) - right) < 4 = Just (left + n, right)
 | otherwise = Nothing

-- landRight :: Birds -> Pole -> Pole
-- landRight n (left, right) = (left, right + n)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
 | abs (left - (right + n)) < 4 = Just (left, right + n)
 | otherwise = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- this is the traditional version (without monad)
--
-- routine :: Maybe Pole
-- routine = case landLeft 1 (0,0) of
--     Nothing -> Nothing
--     Just pole1 -> case landRight 4 pole1 of
--         Nothing -> Nothing
--         Just pole2 -> case landLeft 2 pole2 of
--             Nothing -> Nothing
--             Just pole3 -> landLeft 1 pole3

foo :: Maybe String
foo = Just 3 >>= (\x -> 
      Just "?" >>= (\y ->
      Just (show x ++ y)))

-- with do version
foo2 :: Maybe String
foo2 = do 
    x <- Just 3
    y <- Just "?"
    Just (show x ++ y)

-- routine in do + monad
routine :: Maybe Pole
routine = do 
    start <- return (0,0)
    first <- landLeft 2 start
    -- can throw Nothing here
    -- Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do 
    (x:xs) <- Just "Hello"
    return x


-- failed pattern matching. produce Nothing instead of crash since it is Maybe.
wopwop :: Maybe Char
wopwop = do 
    (x:xs) <- Just ""
    return x 
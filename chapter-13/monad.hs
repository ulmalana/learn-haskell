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


-- list as monad

listOfTuples :: [(Int, Char)]
listOfTuples = do 
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)


class Monad m => MonadPlus m where
    mzero :: m a 
    mplus :: m a -> m a -> m a 

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

sevensOnly :: [Int]
sevensOnly = do 
    x <- [1..50]
    guard ('7' `elem` show x)
    return x 


-- posisi kuda catur

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do 
    (c', r') <- [(c+2,r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1)
                ,(c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

-- without monad
moveKnight2 :: KnightPos -> [KnightPos]
moveKnight2 (c,r) = filter onBoard 
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2) 
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8] 


in3 :: KnightPos -> [KnightPos]
in3 start = do 
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
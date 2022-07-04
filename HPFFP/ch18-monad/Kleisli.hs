import Control.Monad
-- Composing monadic functions

-- In Functor, composing functions is pretty straightforward
-- fmap f . fmap g = fmap (f . g) 

-- We can still compose functions with Monad but in a bit different way

-- composing with join and fmap
mcomp :: Monad m =>
        (b -> m c) ->
        (a -> m b) ->
        a -> 
        m c
mcomp f g a = join (f <$> (g a))

-- composing with bind
mcomp' :: Monad m =>
        (b -> m c) ->
        (a -> m b) ->
        a ->
        m c
mcomp' f g a = g a >>= f -- the order is flipped

-- Example
sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Helo, how old are you?"

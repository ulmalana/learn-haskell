nonsense :: Bool -> Integer
nonsense True = 232
nonsense False = 976

curriedFn :: Integer -> Bool -> Integer
curriedFn i b = i + (nonsense b)

uncurriedFn :: (Integer, Bool) -> Integer
uncurriedFn (i,b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousManuallyNested = 
    \i -> \b -> i + (nonsense b)


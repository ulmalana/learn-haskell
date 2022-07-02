import Control.Applicative ((*>))


-- with do
sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another blah"

-- with monad binding
sequencing' :: IO ()
sequencing' = 
    putStrLn "blah" >>
    putStrLn "another blah"

-- with applicative sequencing
sequencing'' :: IO ()
sequencing'' = 
    putStrLn "blah" *>
    putStrLn "another blah"

-- binding with do syntax
binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

-- binding explicitly
binding' :: IO ()
binding' = 
    getLine >>= putStrLn


-- bind and sequence with do syntax
bindAndSeq :: IO ()
bindAndSeq = do
    putStrLn "your name: "
    name <- getLine
    putStrLn ("your name is: " ++ name)

-- bind and sequence with explicit >>=
bindAndSeq' :: IO ()
bindAndSeq' = 
    putStrLn "your name: " >>
    getLine >>=
    \name -> putStrLn ("your name is:" ++ name)

twoBinds :: IO ()
twoBinds = do
    putStrLn "name: "
    name <- getLine
    putStrLn "age: "
    age <- getLine
    putStrLn ("you are " ++ name ++ " who is " ++ age ++ " years old")

twoBinds' :: IO ()
twoBinds' = 
    putStrLn "name: " >>
    getLine >>=
    \name ->
        putStrLn "age: " >>
        getLine >>=
        \age ->
            putStrLn ("you are " ++ name ++ " and " ++ age)


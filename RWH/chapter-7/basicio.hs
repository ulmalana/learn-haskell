main = do 
    putStr "Greetings! What's your name?: "
    inputStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "."
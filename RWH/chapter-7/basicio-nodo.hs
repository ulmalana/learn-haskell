main =  
    putStrLn "Greeting. What is your name?: " >>
    getLine >>=
        (\inStr -> putStrLn $ "Welcome to Haskell, " ++ inStr ++ ".")

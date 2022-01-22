main = do 
    putStr "Enter a Double: "
    inputStr <- getLine
    let inputDoub = (read inputStr)::Double
    putStrLn ("Twice " ++ show inputDoub ++ " is " ++ show (inputDoub * 2))
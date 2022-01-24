name2reply :: String -> String
name2reply name = 
    "Nice to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do 
    putStr "Greeting again. What's your name?: "
    inStr <- getLine
    let outStr = name2reply inStr
    putStrLn outStr
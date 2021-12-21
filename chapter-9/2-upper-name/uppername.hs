import Data.Char

main = do
    putStrLn "whats your first name?"
    firstName <- getLine
    putStrLn "and your second name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
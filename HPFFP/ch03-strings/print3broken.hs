module Print3Broken where

-- fix
greeting :: String
greeting = "Yarrrr!"

printSecond :: IO ()
printSecond = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond

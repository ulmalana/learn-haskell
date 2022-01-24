import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do
    putStrLn "Is green your fav color? (y/n): "
    inStr <- getLine
    return ((toUpper . head $ inStr) == 'Y')

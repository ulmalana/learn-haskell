import Data.Char (toUpper)

isYes :: String -> Bool
isYes inStr = (toUpper . head $ inStr) == 'Y'

isGreen :: IO Bool
isGreen = do
    putStrLn "is green your fav color?: "
    inStr <- getLine
    return (isYes inStr)
import Data.Char (toUpper)

main = do 
    inStr <- readFile "input.txt"
    writeFile "output.txt" (map toUpper inStr)
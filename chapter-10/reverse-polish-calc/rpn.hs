import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldfn [] . words
    where   foldfn (x:y:ys) "*" = (x * y):ys
            foldfn (x:y:ys) "+" = (x + y):ys
            foldfn (x:y:ys) "-" = (y - x):ys
            foldfn (x:y:ys) "/" = (y / x):ys
            foldfn (x:y:ys) "^" = (y ** x):ys
            foldfn (x:xs) "ln" = log x:xs
            foldfn xs "sum" = [sum xs]
            foldfn xs numberString = read numberString:xs
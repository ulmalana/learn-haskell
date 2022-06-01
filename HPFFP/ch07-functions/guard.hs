-- Guards 

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Right triangle!"
    | otherwise = "not right"

avgGrade :: (Num a, Ord a) => a -> Char
avgGrade x
    | x >= 90 = 'A'
    | x >= 80 = 'B'
    | x >= 70 = 'C'
    | x >= 60 = 'D'
    | x < 60 = 'E'

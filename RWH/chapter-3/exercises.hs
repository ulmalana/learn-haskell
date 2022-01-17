-- NOT FINISHED
import Data.List

-- 1, 2
myLen :: [a] -> Int
myLen [] = 0 
myLen (x:xs) = 1 + myLen xs

-- 3
mean xs = (sum xs) / fromIntegral (length xs)

-- 4
palindrom xs = xs ++ reverse xs

-- 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = if xs == reverse xs
                  then True
                  else False

-- 6
mySort :: [[a]] -> [[a]]
mySort = sortBy (\xs ys -> compare (length xs) (length ys))

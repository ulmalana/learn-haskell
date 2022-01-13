multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- partial function of multThree
multTwoWithNine = multThree 9
--multTwoWithNine 2 3 --> 54

compareWithHundred :: (Num a, Ord a) => a -> Ordering
-- compareWithHundred x = compare 100 x can be written as
compareWithHundred = compare 100

--infix fn
divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a 
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] =[]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
 where g x y = f y x

-- or
-- flip' :: (a -> b -> c) -> b -> a -> c 
-- flip' f x y = f y x

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerSorted = quicksort' (filter (<=x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a 
largestDivisible = head (filter p [100000,99999..])
 where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
 | even n = n:chain (n `div` 2)
 | odd n = n:chain (n*3 +1)


numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
 where isLong xs = length xs > 15

-- with lambda
numLongChains' :: Int 
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

sqrtSums :: Int 
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
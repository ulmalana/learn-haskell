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
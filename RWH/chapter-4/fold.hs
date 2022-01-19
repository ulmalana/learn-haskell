foldl2 :: (a -> b -> a) -> a -> [b] -> a 
foldl2 step zero (x:xs) = foldl2 step (step zero x) xs 
foldl2 _ zero [] = zero

foldr2 :: (a -> b -> b) -> b -> [a] -> b 
foldr2 step zero (x:xs) = step x (foldr2 step zero xs)
foldr2 _ zero [] = zero

myFilter p xs = foldr2 step [] xs
    where step x ys | p x = x : ys
                    | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr2 step [] xs 
    where step x ys = f x : ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a 
myFoldl f z xs = foldr2 step id xs z 
    where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = foldr2 (:) [] xs

append :: [a] -> [a] -> [a]
append xs ys = foldr2 (:) ys xs
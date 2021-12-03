lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry you are not lucky." -- catch all value other than 7

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial s = s * factorial (s - 1)

charName :: Char -> String
charName 'a' = "Adi"
charName 'b' = "Budi"
charName 'c' = "Catur"
-- charName _ = "Unknown" -- Uncomment this line will catch all other char

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "No head in an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This is an empty list"
tell (x:[]) = "The list only has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first 2 elements are: "++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty String"
-- all can be thought as alias
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- Guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
 | bmi <= 18.5 = "You are underweight."
 | bmi <= 25.0 = "You are normal."
 | bmi <= 30.0 = "You are overweight."
 | otherwise = "You are obese."

bmiTellv2 :: (RealFloat a) => a -> a -> String
bmiTellv2 weight height
 | weight / height ^ 2 <= 18.5 = "You are underweight."
 | weight / height ^ 2 <= 25.0 = "You are normal."
 | weight / height ^ 2 <= 30.0 = "You are overweight."
 | otherwise = "You are obese."

max' :: (Ord a) => a -> a -> a
max' a b 
 | a > b = a 
 | otherwise = b 

bmiTellv3 :: (RealFloat a) => a -> a -> String
bmiTellv3 weight height
 | bmi <= 18.5 = "You are underweight."
 | bmi <= 25.0 = "You are normal."
 | bmi <= 30.0 = "You are overweight."
 | otherwise = "You are obese."
 where bmi = weight / height ^ 2

bmiTellv4 :: (RealFloat a) => a -> a -> String
bmiTellv4 weight height
 | bmi <= skinny = "You are underweight."
 | bmi <= normal = "You are normal."
 | bmi <= fat = "You are overweight."
 | otherwise = "You are obese."
 where bmi = weight / height ^ 2
       skinny = 18.5
       normal = 25.0
       fat = 30.0

initials :: String -> String -> String
initials fname lname = [f] ++ ". " ++ [l] ++ "."
 where (f:_) = fname
       (l:_) = lname


calcBmi :: (RealFloat a) => [(a, a)] -> [a]
calcBmi xs = [bmi w h | (w, h) <- xs]
 where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h 
	    topArea = pi * r ^ 2
	in sideArea + 2 * topArea


-- Case

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
 where what [] = "empty."
       what [x] = "a singleton."
       what xs = "a longer list."
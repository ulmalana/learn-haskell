import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Geometry
import qualified Geometry.Sphere as Sphere 
import qualified Geometry.Cuboid as Cuboid 
import qualified Geometry.Cube as Cube

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- caesar chiper
encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook = 
    [("betty","12345"),
     ("betty", "123456"),
     ("bonnie", "23456"),
     ("patsy", "34567"),
     ("patsy", "345678"),
     ("lucille", "45678"),
     ("wendy", "56789"),
     ("penny", "67890"),
     ("penny", "678901")]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v 
findKey2 key [] = Nothing
findKey2 key ((k,v):xs) = if key == k 
                             then Just v 
                             else findKey2 key xs

findKey3 :: (Eq k) => k -> [(k, v)] -> Maybe v 
findKey3 key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

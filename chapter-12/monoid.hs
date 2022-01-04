import Data.Monoid

lengthCompare1 :: String -> String -> Ordering
lengthCompare1 x y = let a = length x `compare` length y
                         b = x `compare` y
                     in if a == EQ then b else a

-- using monoid
lengthCompare2 :: String -> String -> Ordering
lengthCompare2 x y = (length x `compare` length y) `mappend`
                     (x `compare` y)
module QC where

import Test.QuickCheck
import Data.List
import Prettify

instance Arbitrary Doc where
    arbitrary = do 
        n <- choose (1,6) :: Gen Int 
        case n of 
            1 -> return Empty
            2 -> do x <- arbitrary
                    return (Char x)
            3 -> do x <- arbitrary
                    return (Text x)
            4 -> return Line
            5 -> do x <- arbitrary
                    y <- arbitrary
                    return (Concat x y)
            6 -> do x <- arbitrary
                    y <- arbitrary
                    return (Union x y)   

prop_empty_id x = 
    empty <-> x == x 
    && 
    x <-> empty == x

prop_char c  = char c == Char c

prop_text s = text s == if null s then Empty else Text s 

prop_line = line == Line

prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where glue [] = empty
          glue (d:ds) = d <-> glue ds

prop_punctuate s xs = punctuate s xs == intersperse s xs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where combine [] = []
          combine [x] = [x]
          combine (x:Empty:ys) = x : combine ys
          combine (Empty:y:ys) = y : combine ys
          combine (x:y:ys) = x `Concat` y : combine ys

prop_mempty_id x = 
    mempty `mappend` x == x 
    && 
    x `mappend` mempty == (x :: Doc)



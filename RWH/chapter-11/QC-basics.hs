import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort []    = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs 
          rhs = filter (>= x) xs

-- idempotent test
prop_idempotent xs = qsort (qsort xs) == qsort xs

-- will fail with empty list
prop_minimum xs = head (qsort xs) == minimum xs

-- check the list first
prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs  = ordered (qsort xs)
    where ordered []    = True
          ordered [x]   = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maxixum xs = not (null xs) ==>
                    last (qsort xs) == maximum xs

prop_append xs ys = not (null xs) ==>
                      not (null ys) ==>
                        head (qsort xs ++ ys) == min (minimum xs) (minimum ys)

-- test against (standard/built) model
prop_sort_model xs = sort xs == qsort xs
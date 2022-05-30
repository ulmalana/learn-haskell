module TupleFn where

addEm :: Num a => (a, a) -> a
addEm (x, y) = x + y

-- not pattern matching
addEm' :: Num a => (a, a) -> a
addEm' tup = (fst tup) + (snd tup)

fst3 :: (a, b ,c) -> a
fst3 (x, _, _) = x

third3 :: (a,b,c) -> c
third3 (_, _, x) = x

f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (x,y,z) (k,l,m) = ((x,k), (z, m))

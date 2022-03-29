sum' []     = 0
sum' (n:ns) = n + sum' ns

qsort :: Ord a => [a] -> [a]
qsort []        = []
qsort (x:xs)    = qsort smaller ++ [x] ++ qsort larger
                  where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

seqn :: [IO a] -> IO [a]
seqn []         = return []
seqn (act:acts) = do
                    x <- act
                    xs <- seqn acts
                    return (x:xs)

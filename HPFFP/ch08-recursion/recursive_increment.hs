-- normal increment function
inc :: Num a => a -> a
inc = (+1)

-- three times recursive increment 
three = inc . inc . inc $ 0

-- increment in indefinite number of times
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

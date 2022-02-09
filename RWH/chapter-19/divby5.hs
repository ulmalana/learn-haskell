divBy :: Integral a => a -> [a] -> Maybe [a]
divBy = divByGeneric

divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = fail "div by zero"
divByGeneric numerator (denom:xs) = 
    do next <- divByGeneric numerator xs
       return ((numerator `div` denom):next)
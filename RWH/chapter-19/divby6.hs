divBy :: Integral a => a -> [a] -> Either String [a]
divBy _ [] = Right []
divBy _ (0:_) = Left "divBy: div by zero"
divBy numerator (denom:xs) = 
    case divBy numerator xs of
        Left x -> Left x 
        Right result -> Right ((numerator `div` denom):result)
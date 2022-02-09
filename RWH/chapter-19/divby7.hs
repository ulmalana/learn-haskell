data DivByError a = DivBy0 | ForbiddenDenominator a deriving (Eq, Show, Read)

divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy _ [] = Right []
divBy _ (0:_) = Left DivBy0
divBy _ (10:_) = Left (ForbiddenDenominator 10)
divBy _ (20:_) = Left (ForbiddenDenominator 20)
divBy numerator (denom:xs) = 
    case divBy numerator xs of
        Left x -> Left x 
        Right result -> Right ((numerator `div` denom):result)
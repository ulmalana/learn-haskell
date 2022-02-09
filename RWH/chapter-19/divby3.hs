divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator denom =
    map worker denom
    where worker 0 = Nothing
          worker x = Just (numerator `div` x)
myLookUp :: Eq a => a -> [(a,b)] -> Maybe b 
myLookUp _ [] = Nothing
myLookUp key ((thiskey, thisval):rest) = 
    if key == thiskey
        then Just thisval
        else myLookUp key rest
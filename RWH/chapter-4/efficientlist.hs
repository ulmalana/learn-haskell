-- alt head: not efficient since length has to walk the entire list
dumbExample xs = if length xs > 0 
                 then head xs 
                 else '0'

-- efficient alt head
smartExample xs = if not (null xs)
                  then head xs
                  else '0'

smartExample2 (x:_) = x 
smartExample2 [] = '0'
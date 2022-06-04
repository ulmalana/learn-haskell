xs = map show [1..5]

showFoldRight = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs

showFoldLeft = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" xs

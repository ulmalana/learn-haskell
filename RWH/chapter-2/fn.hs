add a b = a + b

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = last $ init xs
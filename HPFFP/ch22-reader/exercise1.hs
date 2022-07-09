import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled xs = (cap xs, rev xs)

tupledA :: [Char] -> ([Char], [Char])
tupledA = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
    c <- cap
    r <- rev
    return (c, r)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \c -> rev >>= \r -> return (c, r)

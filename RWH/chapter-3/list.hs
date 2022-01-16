data List a = Cons a (List a) | Nil deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- exercise
toList :: List a -> [a]
toList (Cons x (cons)) = x:toList cons
toList Nil = []
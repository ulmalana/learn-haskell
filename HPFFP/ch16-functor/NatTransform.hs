{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a

-- this will work since we dont change the content
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- this will fail since we change content
--degenerateNat :: Nat Maybe []
--degenerateNat Nothing = []
--degenerateNat (Just a) = [a+1]



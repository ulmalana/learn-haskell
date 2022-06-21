-- Functor

-- this type has kind *
data FixMePls = FixMe | Pls deriving (Eq, Show)

-- creating Functor instance for kind * is not possible
-- instance Functor FixMePls where
--    fmap = error "it's ok it wont compile"

-- this type has kind * -> *
data FixMePls' a = FixMe' | Pls' a deriving (Eq, Show)

-- this instance creation will compile since FixMePls has kind * -> *
instance Functor FixMePls' where
    fmap _ FixMe' = FixMe'
    fmap f (Pls' a) = Pls' (f a)

-- creating Functor instance this way will fail as well
-- since (FixMePls' a) has kind *.
-- instance Functor (FixMePls' a) where
--    fmap _ FixMe' = FixMe'
--    fmap f (Pls' a) = Pls' (f a)

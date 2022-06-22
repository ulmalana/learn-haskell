-- Functor for Higher kinded type

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

-- This will fail since Two has kind * -> * -> *
-- while Functor needs type with kind * -> *
-- instance Functor Two where
--    fmap = undefined

-- This will fail with the same reason
-- instance Functor Or where
--    fmap = undefined

-- Applying one argument to Two will change its kind to * -> *
-- and make it work
--instance Functor (Two a) where
--    fmap = undefined

--instance Functor (Or a) where
--    fmap = undefined

-- Then, applying function f to type argument a will fail
-- instance Functor (Two a) where
--    fmap f (Two a b) = Two $ (f a) (f b)

-- We need to ignore the first type argument that has been applied
-- and it will work
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

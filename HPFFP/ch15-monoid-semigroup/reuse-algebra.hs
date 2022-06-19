data Booly a = Falsy | Trueish deriving (Eq, Show)

-- TODO: define a Semigroup instancr for Booly

-- even though Booly has a parameter, we dont need to put
-- monoid constraint since we dont use the parameter in the
-- data constructor
-- instance Monoid (Booly a) where 
--    mappend Falsy _ = Falsy
--    mappend _ Falsy = Falsy
--     mappend Trueish Trueish = Trueish

data Optional a = Nada | Only a deriving (Eq, Show)

-- to create a monoid instance for Optional, we need to define Semigroup
-- instance for Optional
instance Semigroup a => Semigroup (Optional a) where
    (Only a) <> (Only b) = Only $ a <> b
    (Only a) <> Nada = Only a
    Nada <> (Only b) = Only b
    Nada <> Nada = Nada

-- because we Only data constructor has a parameter, we need 
-- to put monoid constraint so that we can make sure parameter a
-- also support monoid
instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

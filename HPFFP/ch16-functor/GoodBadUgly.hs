-- Example of breaking Functor laws

-- identity law
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- law-abiding
instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

-- law-breaking since it fmap returns different value in case of type constants
-- instance Functor WhoCares where
--    fmap _ ItDoesnt = WhatThisIsCalled
--    fmap _ WhatThisIsCalled = ItDoesnt
--    fmap f (Matter a) = Matter (f a)

-- composition law
data Counting a = Heisenberg Int a deriving (Eq, Show)

-- law-breaking
-- instance Functor Counting where
--    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

-- law-abiding
-- just leave the Int alone
instance Functor Counting where
    fmap f (Heisenberg n a) = Heisenberg (n) (f a)

-- {-# LANGUAGE DeriveAnyClass #-}

module MonadTest where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

-- alternative Applicative instance for other monoid
-- need to modify the Monad instance as well.
-- 
-- instance Applicative CountMe where
--      pure = CountMe 1
--      CountMe n f <*> CountMe n' a = CountMe (n * n') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f = 
        let CountMe n' b = f a
        in CountMe (n+n') b -- change this to (n*n') to support alternative instance

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

main = do
    let trigger = undefined :: CountMe (Int, String, Int)
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

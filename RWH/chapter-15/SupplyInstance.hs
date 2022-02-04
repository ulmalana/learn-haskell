{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies#-}

import SupplyClass
import RandomSupply
import Control.Monad
-- import Control.Monad.Fail (MonadFail)

newtype Reader e a = R { runReader :: e -> a }

newtype MySupply e a = MySupply { runMySupply :: Reader e a } deriving (Monad)

instance Functor (Reader a) where
    fmap = liftM

instance Applicative (Reader a) where
    pure a = R $ \_ -> a
    (<*>) = ap

instance Monad (Reader a) where
    return a = R $ \_ -> a 
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

instance Functor (MySupply a) where
    fmap = liftM

instance Applicative (MySupply a) where
    pure = return
    (<*>) = ap 

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)
    -- or: next = MySupply (Just `liftM` ask)

ask :: Reader e e 
ask = R id

xy :: (Num s, MonadSupply s m) => m (Maybe s)
xy = do
  mx <- next
  my <- next
  return $ liftM2 (*) mx my

runMS :: MySupply i a -> i -> a 
runMS = runReader . runMySupply
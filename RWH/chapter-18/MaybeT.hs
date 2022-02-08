{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module MaybeT where

import Control.Monad (ap, liftM)
import Control.Monad.Trans
import Control.Monad.State 

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do 
                    unwrapped <- runMaybeT x
                    case unwrapped of 
                        Nothing -> return Nothing
                        Just y -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a 
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a 
failMT _ = MaybeT $ return Nothing

-- instance (Monad m) => Functor (MaybeT m) where
--     fmap = liftM

-- instance (Monad m) => Applicative (MaybeT m) where
--     pure = returnMT
--     (<*>) = ap

instance (Functor f) => Functor (MaybeT f) where
    fmap f x = MaybeT $ fmap f <$> runMaybeT x 

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure x = MaybeT $ pure (Just x)
    f <*> x = MaybeT $ do 
                f' <- runMaybeT f
                x' <- runMaybeT x 
                case (f', x') of 
                    (Just f'', Just x'') -> return (Just (f'' x''))
                    _ -> return Nothing
                    
instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    (>>=) = bindMT
    fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)

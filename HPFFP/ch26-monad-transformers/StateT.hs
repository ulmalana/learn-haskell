newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT smas) = StateT $ (fmap . fmap) (\(a,s) -> (f a, s)) smas

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)

    (StateT smfs) <*> (StateT smas) = 
        StateT $ \s -> do
            (f, ns) <- smfs s
            (a, fs) <- smas ns
            return (f a, fs)

instance Monad m => Monad (StateT s m) where
    return = pure

    (StateT smas) >>= f =
        StateT $ \s -> do
            (a, ns) <- smas s
            runStateT (f a) ns

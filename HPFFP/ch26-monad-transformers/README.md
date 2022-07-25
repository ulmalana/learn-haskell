# Chapter 26 - Monad Transformers

## `MaybeT`

`MaybeT` transformer is a bit more complex than `IdentityT`. However, their
Functor and Applicative instances are similar.

```
    -- for comparison
    newtype Compose f g a =
        Compose { getCompose :: f (g a) }

    -- This structure is similar to Compose
    newtype MaybeT m a =
        MaybeT { runMaybeT :: m (Maybe a) }
```

We dont need to do anything different for Functor instance:
```
    -- for comparison
    instance (Functor f, Functor g) =>
                Functor (Compose f g) where
        fmap f (Compose fga) =
            Compose $ (fmap . fmap) f fga

    -- MaybeT
    instance (Functor m) => Functor (MaybeT m) where
        fmap f (MaybeT ma) = 
            MaybeT $ (fmap . fmap) f ma
```

That's the case for Applicative instance:
```
    -- for comparison
    instance (Applicative f, Applicative g) =>
                Applicative (Compose f g) where
        pure x = Compose $ (pure . pure) x
        Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x

    -- MaybeT
    instance (Applicative m) => Applicative (MaybeT m) where
        pure x = MaybeT $ (pure . pure) x
        (MaybeT fab) <*> (MaybeT mma) =
            MaybeT $ (<*>) <$> fab <*> mma
```

Lastly, for Monad instance:

```
    instance (Monad m) => Monad (MaybeT m) where
        return = pure
        
        (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
        (MaybeT ma) >>= f =
            MaybeT $ do
                v <- ma
                case v of
                    Nothing -> return Nothing
                    Just y -> runMaybeT (f y)
```

## `EitherT`

`Either` also has its own transformer.

```
    newtype EitherT e m a = 
        EitherT { runEitherT :: m (Either e a) }
```

Functor instance for `EitherT`:

```
    instance Functor m => Functor (EitherT e m) where
        fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea
```

Then, Applicative instance:

```
   instance Applicative m => Applicative (EitherT e m) where
        pure x = EitherT $ (pure . pure) x

        (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea 
```

Finally, Monad instance of `EitherT`:

```
    instance Monad m => Monad (EitherT e m) where
        return = pure
    
        (EitherT mea) >>= f = EitherT $ do
            ea <- mea
            case ea of
                Left e -> return $ Left e
                Right a -> runEitherT $ f a
```

## `ReaderT`

```
	newtype ReaderT r m a = 
    	ReaderT { runReaderT :: r -> m a }

	instance Functor m => Functor (ReaderT r m) where
    	fmap f (ReaderT rma) =
        	ReaderT $ (fmap . fmap) f rma

	instance Applicative m => Applicative (ReaderT r m) where
    	pure x = ReaderT $ (pure . pure) x

    	(ReaderT fmab) <*> (ReaderT rma) =
        	ReaderT $ (<*>) <$> fmab <*> rma

	instance Monad m => Monad (ReaderT r m) where
    	return = pure

    	(>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    	(ReaderT rma) >>= f =
	        ReaderT $ \r -> do
    	        a <- rma r
        	    runReaderT (f a) r

```
## `StateT`

```
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
```
## MonadTrans

MonadTrans is a **typeclass** with one core method: `lift`. It is about lifting
actions in some Monad over a transformer type which wraps itself in the
original Monad.

```
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

```

## MonadIO

MonadIO is intended to **keep lifting** IO action until it is lifted over all
structure.
```
class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a
```

{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
    Identity { runIdentity :: a }

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

newtype One f a = One (f a) deriving (Eq, Show)

newtype Three f g h a =
    Three (f (g (h a))) deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance Functor f => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

instance (Functor f, Functor g, Functor h) =>
            Functor (Three f g h) where
    fmap f (Three fgha) = 
        Three $ (fmap . fmap . fmap) f fgha

instance (Applicative f, Applicative g) =>
            Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgf) <*> (Compose fga) = Compose $ (<*>) <$> fgf <*> fga

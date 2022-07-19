# Chapter 25 - Composing Types

Composing two Functors or Applicatives can return another Functor or
Applicative. However, composing two Monads does not necessarily return another
Monad.

## Common functions as types

### Identity

```
    newtype Identity a =
        Identity { runIdentity :: a}
```

`run` or `get` prefixes indicate the accessor functions for extracting the
underlying value.

In GHCI, kind of `Identity` is similar to type signature of `id`

```
    > :t id
    id :: a -> a
    
    > :k Identity
    Identity :: * -> *
```

### Compose

`Compose` type below represents a datatype that corresponds to **function
composition**. `f` and `g` are **type constructor**, not term-level functions.

```
    newtype Compose f g a =
        Compose { getCompose :: f (g a) }
        deriving (Eq, Show)
```

`Compose` type above also has a kind similar to type signature of `(.)`

```
    > :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c

    > :k Compose
    Compose :: (* -> *) -> (* -> *) -> * -> *

    > :t Compose [Just 1, Nothing]
    Compose [Just 1, Nothing] :: Compose [] Maybe a

    > :t Compose [Just (1 :: Int), Nothing]
    Compose [Just (1 :: Int), Nothing] :: Compose [] Maybe Int
```

* `f` ~ `[]`
* `g` ~ `Maybe`
* `a` ~ `Int`

## Composing Functors

### Functor instance for `Identity`

```
    instance Functor Identity where
        fmap f (Identity a) = Identity (f a)
```

### Functor instance for `Compose`

We need to fmap twice to get the value inside the layered structures.

```
    instance (Functor f, Functor g) => Functor (Compose f g) where
        fmap f (Compose fga) = Compose $ (fmap .fmap) f fga
        
```

Trying to mapping a function to `Compose`:

```
    > fmap (+1) (Compose [Just 1, Just 2, Nothing])
    Compose {getCompose = [Just 2, Just 3, Nothing]}
```

Different number of layered structure with its instance:

```
    -- one layer of wrapping
    newtype One f a = 
        One (f a) deriving (Eq, Show)

    instance Functor f => Functor (One f) where
        fmap f (One fa) = One $ fmap f fa


    -- three layers of wrapping
    newtype Three f g h a = 
        Three (f (g (h a))) deriving (Eq, Show)

    instance (Functor f, Functor g, Functor h) =>
                Functor (Three f g h) where
        fmap f (Three fgha) = 
            Three $ (fmap . fmap . fmap ) f fgha
        
```

## Composing Applicative

Composing Applicatives creates a new Applicative instance.

```
    {-# LANGUAGE InstanceSigs #-}

    instance (Applicative f, Applicative g) =>
                Applicative (Compose f g) where
        pure :: a -> Compose f g a
        pure a = Compose $ (pure . pure) a

        (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
        (Compose fgf) <*> (Compose fga) = Compose $ (<*>) <$> fgf <*> fga
```

## Composing Monads

There is no problem composing two types that have Monad instances, but the
result of that is not a Monad. This happens because of lacks of information.
`Compose` has a polymorphic parameter, so when we want to combine two Monads we
will combine two polymorphic binds into a single combined bind, whichs is
impossible.

To get a Monad instance for combination of types we need to use **monad
transformer**.

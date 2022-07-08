# Chapter 21 - Traversable

`Traversable` is a way to **traverse a data structure**, **mapping a function**
inside a structure while **accumulating the applicative contexts** along the way.

## Traversable typeclass

* ```
    class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f =>
                    (a -> f b)
                 -> t a
                 -> f (t b)
        traverse f = sequenceA . fmap f

        sequenceA :: Applicative f => t (f a) -> f (t a)
        sequenceA = traverse id
  ```
* Minimal implementation for `Traversable` is either `traverse` or `sequenceA`

## `sequenceA`

* `sequenceA` **flips two contexts or structures** and doesnt apply any
  function to the values inside the structure.
    * ```
        > fmap Just [1,2,3]
        [Just 1, Just 2, Just 3]
        > sequenceA $ fmap Just [1,2,3]
        Just [1,2,3]

        > sequenceA [Just 1, Just 2, Just 3]
        Just [1,2,3]
        > sequenceA [Just 1, Just 2, Nothing]
        Nothing

        > fmap sum $ sequenceA [Just 1, Just 2, Just 3]
        Just 6
        > fmap product (sequenceA [Just 3, Just 4, Nothing])
        Nothing
      ```
* `Data.Maybe` has `catMaybes` function to handle a list of `Maybe` values.
    * ```
        > catMaybes [Just 1, Just 2, Just 3]
        [1,2,3]
        > catMaybes [Just 1, Just 2, Nothing]
        [1,2]
        > sum $ catMaybes [Just 1, Just 2, Just 3, Nothing]
        6
        > fmap sum $ sequenceA [Just 1, Just 2, Just 3, Nothing]
        Nothing
      ```

## `traverese`

* `traverse` still maps a function over some embedded values (like `fmap`), but
  then flips the two structure around as `sequenceA` does.
    * `traverse f = sequenceA . fmap f`
    * ```
        > fmap Just [1,2,3]
        [Just 1, Just 2, Just 3]
        > sequenceA $ fmap Just [1,2,3]
        Just [1,2,3]
        > sequenceA . fmap Just $ [1,2,3]
        Just [1,2,3]
        > traverse Just [1,2,3]
        Just [1,2,3]
      ```

## What is traversable for?

* Anytime **we need to flip two constructors around**, or **map something then flip
  them around**.
* Example
    * ```
        -- we want to map a function over some structure.
        -- the detail implementation is irrelevant for now.
        > let f = undefined :: a -> Maybe b
        > let xs = undefined :: [a]
        > :t map f xs
        map f xs :: [Maybe b]

        -- then, what if we want the value to be Maybe [b] type?
        > :t sequenceA $ map f xs
        sequenceA $ map f xs :: Maybe [a]

        -- we can simplify with traverse.
        > :t traverse f xs
        traverse f xs :: Maybe [b]
      ```

## Traversable instances

### Either

```
    data Either a b = Left a | Right b deriving (Eq, Ord, Show)

    instance Functor (Either a) where
        fmap _ (Left x) = Left x
        fmap f (Right y) = Right (f y)

    instance Applicative (Either a) where
        pure = Right
        Left e <*> _ = Left e
        Right f <*> r = fmap f r

    instance Foldable (Either a) where
        foldMap _ (Left _) = mempty
        foldMap f (Right y) = f y
    
        foldr _ z (Left _) = z
        foldr f z (Right y) = f y z

    instance Traversable (Either a) where
        traverse _ (Left x) = pure (Left x)
        traverse f (Right y) = Right <$> f y
```

### Tuple

```
    instance Functor ((,) a) where
        fmap f (x,y) = (x, f y)

    instance Monoid a => Applicative ((,) a) where
        pure x = (mempty, x)
        (u, f) <*> (v, x) = (u `mappend` v, f x)

    instance Foldable ((,) a) where
        foldMap f (_, y) = f y
        foldr f z (_, y) = f y z

    instance Traversable ((,) a) where
        traverse f (x, y) = (,) x <$> f y
```

## Traversable laws

### traverse
* Naturality
    * `t . traverse f =  traverse (t . f)`

* Identity
    * `traverse Identity = Identity`

* Composition
    * `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

### sequenceA
* Naturality
    * `t . sequenceA = sequenceA . fmap t`
* Identity
    * `sequenceA . fmap Identity = Identity`
* Composition
    * `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`

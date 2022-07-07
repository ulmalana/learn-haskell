# Chapter 20 - Foldable

## Foldable class

`Foldable` class is a class of data structures that **can be folded** to a
**summary value**. `foldMap` of `foldr` is the minimal complete definition of
`Foldable` typeclass.

## Monoids in folding

Monoids play an important role in folding. Folding necessarily implies **a
binary associative operation** that has an identity value. The first two
operations defined in Foldable:

```
    class Foldable (t :: * -> *) where
        fold :: Monoid m => t m -> m
        foldMap :: Monoid m => (a -> m) -> t a -> m
```

`fold` allows us to combine elements inside a `Foldable` structure **using
Monoid** defined for those elements. `foldMap` first maps each element to a
Monoid then combines them using that instance of Monoid.

```
    > foldr (+) 0 [1..5]
    15
    > fold (+) [1,2,3,4,5]
    ERROR
    > fold [1,2,3,4,5]
    ERROR
    > fold [Sum 1, Sum 2, Sum 3, Sum 4, Sum 5]
    Sum {getSum = 15}
    > fold [1,2,3,4,5 :: Sum Integer]
    Sum {getSum = 15}
    > fold [1,2,3,4,5 :: Product Integer]
    Product {getProduct = 120}
    > fold ["halo", " riz"]
    "halo riz"

    > foldMap Sum [1,2,3,4,5]
    Sum {getSum = 15]
    > foldMap Product [1,2,3,4,5]
    Product {getProduct = 120}
    > foldMap Any [(3 == 4), (9 > 6)]
    Any {getAny = True}
    > foldMap First [Just 1, Nothing, Just 5]
    First {getFirst = Just 1}
    > foldMap Last [Just 1, Nothing, Just 5]
    Last {getLast = Just 5}


    -- folding with non-data constructor (ie ordinary function)
    > foldMap (*5) [1,2,3 :: Product Integer]
    Product {getProduct = 750}
    -- 5 * 10 * 15

    > foldMap (*5) [1,2,3 :: Sum Integer]
    Sum {getSum = 30}
    -- 5 + 10 + 15

    > foldMap (*5) (Just 5) :: Sum Integer
    Sum {getSum = 25}
    > foldMap (*5) Nothing :: Sum Integer
    Sum {getSum = 0}
    > foldMap (*5) Nothing :: Product Integer
    Product {getProduct = 1}
```

## Some basic derived operations

* `toList :: t a -> [a]`
    * ```
        > toList (Just 1)
        [1]
        > map toList [Just 1, Just 2, Just 3]
        [[1], [2], [3]]
        > concatMap toList [Just 1, Just 2, Nothing]
        [1,2]
        > toList (1,2)
        [2] -- only compute the rightmost value
      ```
* `null :: t a -> Bool`
    * ```
        > null (Left 3)
        True
        > null Nothing
        True
        > fmap null [Just 1, Just 2, Nothing]
        [False, False, True]
      ```

* `length :: t a -> Int`
    * ```
        > length (1,2)
        1
        > length [(1,2), (3,4), (5,6)]
        3
        > fmap length [(1,2), (3,4), (5,6)]
        [1,1,1]
        > fmap length Just [1,2,3]
        1
        > (fmap . fmap) length Just [1,2,3]
        Just 3
      ```

* `elem :: Eq a => a -> t a -> Bool`
    * ```
        > elem 2 (Just 3)
        False
        > elem True (Left True)
        False
        > elem True (Right True)
        True
        > fmap (elem 3) [Right 3, Right 2, Right 3]
        [False, False, True]
      ```

* `maximum :: forall a . Ord a => t a -> a` and `minimum :: forall a . Ord a =>
  t a -> a`
    * ```
        > maximum [10,11,12,13]
        33
        > fmap maximum [Just 2, Just 10, Just 5]
        [2,10,5]
        > fmap maximum (Just [3,7,10,2])
        Just 10
        > fmap minimum [Just 4, Just 3, Nothing]
        Exception: minimum: empty structure
        > minimum (Left 3)
        Exception: minimum: empty structure
      ```

* `sum :: (Foldable t, Num a) => t a -> a` and `product :: (Foldable t, Num a)
  => t a -> a`
    * ```
        > sum (7,5)
        5
        > fmap sum [(7,5), (3,4)]
        [5,4]
        > fmap sum (Just [1,2,3,4,5])
        Just 15
        > product Nothing
        1
        > fmap product (Just [])
        1
        > fmap product (Right [1,2,3])
        Right 6
      ```

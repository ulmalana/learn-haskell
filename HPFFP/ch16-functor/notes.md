# Chapter 16 - Functor

## What is it?

* A functor is a way to **apply a function** over some **structure we dont want to
  alter**.
    * apply the function to the value inside the structure and leave the
      structure alone.
* `Functor` typeclass
    * ```
        class Functor f where
            fmap :: (a -> b) -> f a -> f b
      ```
* `fmap` is a generalized version of `map` (which can only be used with list).
    * `fmap` **ignores** **first value in Tuple** and `Left` in `Either`
    * `fmap :: (a -> b) -> (e,) a -> (e,) b`
    * `fmap :: (a -> b) -> Either e a -> Either e b`
* Infix operator of `fmap`
    * `(<$>) :: Functor f => (a -> b) -> f a -> f b`
* Types that want to have Functor instance should have kind **\* -> \*** (have
  parameter, not type constant).
    * If we want to create Functor instance for type constant, it is just
      function application

## Functor Laws

### Identity
* If we `fmap` the identity function, it should have the same result as passing
  our value to identity
* `fmap id == id`

### Composition
* fmapping two composed functions over some structure should produce the same
  result as fmapping each function and compose them:
* `fmap (f . g) == fmap f . fmap g`

## Stacked Functor and Lifted Function

We can lift the function **more than once** to alter **the contained values in a
structure of a structure**. Example: working with list of `Maybe String`
* ```
    > let lms = [Just "Ave", Nothing, Just "Woohoo"]
    > let replaceWithP = const 'p'
    > replaceWithP lms
    'p'

    -- altering the content of the list
    > fmap replaceWithP lms
    "ppp"

    -- lifting twice for altering the content of Maybe inside the list
    > (fmap . fmap) replaceWithP lms 
    [Just 'p', Nothing, Just 'p']

    -- lifting thrice for altering the string inside Maybe inside the list
    > (fmap . fmap . fmap) replaceWithP lms
    [Just "ppp", Nothing, Just "pppppp"]
  ```
* More examples
    * ```
        > let replaceWithP = const 'p'
        > let ha = Just ["Ha", "Ha"]
        > let lmls = [ha, Nothing, Just []]
        
        -- lifting once to alter the content of list
        > fmap replaceWithP lmls
        "ppp"

        -- lifting twice to alter the content of Maybe inside the list
        > (fmap . fmap) replaceWithP lmls
        [Just 'p', Nothing, Just 'p']

        -- lifting thrice to alter the content of ha inside Maybe inside the list
        > (fmap . fmap .fmap) replaceWithP lmls
        [Just "pp", Nothing, Just ""]

        -- lifting 4x to alter the String in ha inside Maybe inside the list
        > (fmap . fmap . fmap . fmap) replaceWithP lmls
        [Just ["pp", "pp"], Nothing, Just []]
      ```

## Functor for higher-kinded types

* Previously, Functor only works with type that has kind **\* -> \***.
* However, it doesnt work with types that have higher kind than **\* -> \***.
    * Example: **Tuple (a,b)**, `Either a b`, and `Heisenberg` have kind **\*
      -> \* -> \***
    * We have to deal only with one type argument.
* Applying the first type argument will change the kind to **\* -> \***.
* Thus, in Tuple and `Either` we only transform the second argument and ignore the first argument.

## Ignoring possibilities

* Functor for `Maybe` and `Either` can be used to ignore error or failure cases 
  because it doesnt touch the leftmost value.
* Instead of creating function with pattern matching, we can use `fmap`.
    * ```
        -- without fmap
        incIfJust :: Num a => Maybe a -> Maybe a
        incIfJust (Just n) = Just $ n + 1
        incIfJust Nothing = Nothing

        -- with fmap
        incMaybe :: Num a => Maybe a -> Maybe a
        incMaybe m = fmap (+1) m

        -- lifted version
        liftedInc :: (Functor f, Num b) => f b -> f b
        liftedInc = fmap (+1)
      ```

## IO Functor

* `IO` is an abstract datatype and has no data constructor to pattern match.
* We can only work with typeclasses `IO` provides
* Example
    * ```
        getInt :: IO Int
        getInt = fmap read getLine

        > fmap (+1) getInt
        10
        11
      ```

## Something Different: Natural transformation

* Functor is about lifting functions over a structure for transforming the
  contents and leave the structure alone.
* What if we want to **transform only the structure** and **leave the content alone**?
    * It is called **natural transformation**
* We can define natural transformation with the following code
    * ```
        {-# LANGUAGE RankNTypes #-}

        type Nat f g = forall a . f a -> g a
      ```
    * the quantification `forall` is usefule for ensuring that the content is
      not changed, only the structure `f` and `g`.
    * language pragma RankNTypes is used for supporting `forall`

## Functors are unique for a given datatype

* For Monoid, a datatype may have multiple Monoid instances (differentiate
  using `newtype`)
* However, for Functor, a datatype has a unique Functor instance.

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

# Chapter 18 - Monad

Monad is an **applicative functor with some unique features**.

* Monad typeclass
    * ```
        class Applicative m => Monad m where
            (>>=) :: m a -> (a -> m b) -> m b
            (>>) :: m a -> m b -> m b
            return :: a -> m a
      ```

* We can write `fmap` using **monadic operations**
    * `fmap f xs = xs >>= return . f`
    * `fmap (+1) [1..3]` -> `[2,3,4]`
    * `[1..3] >>= return . (+1)` -> `[2,3,4]`

## Novel part of Monad

* Comparison between Functor, Applicative, and Monad
    * ```
        fmap :: Functor f       =>   (a -> b) -> f a        -> f b
        <*>  :: Applicative f   => f (a -> b) -> f a        -> f b
        >>=  :: Monad f         => f  a       -> (a -> f b) -> f b
      ```
* The novel part of Monad is the ability to flatten the structure like `concat`
  with `join`.
    * `concat :: Foldable t => t [a] -> [a]`
    * `join :: Monad m => m (m a) -> m a`
* `bind` function in terms of `fmap` and `join`
    * ```
        bind :: Monad m => (a -> m b) -> m a -> m b
        bind f = join . fmap f
      ```
## What Monad is not
* Monad is **not**:
    * **Impure**: Monadic functions are pure functions. `IO` is an abstract
      datatype that allows for impure actions.
    * **An embedded language for imperative programming.**
    * **A value**. It is a typeclass.
    * **About strictness**. `bind` and `return` are nonstrict.

## Lifting in Monad
* Lifting function in Monad is similar to Applicative
    * ```
        liftA :: Applicative f => (a -> b)  -> f a  -> f b
        liftM :: Monad m       => (a1 -> r) -> m a1 -> m r
      ```
* Similarity between `zipWith` and `liftA2` or `liftM2`
    * ```
        > :t zipWith
        zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        > zipWith (+) [3,4] [5,6]
        [8,10]
        > liftA2 (+) [3,4] [5,6]
        [8,9,9,10]
        > liftM2 (+) [3,4] [5,6]
        > [8,9,9,10]
      ```
    * The differing behaviour has to do with **which list monoid is being used**.

## Do syntax

* Do syntax is the sugar version of binding sequence of actions.
* Sequencing two actions in `Applicative` and `Monad`
    * ```
        (*>) :: Applicative f => f a -> f b -> f b
        (>>) :: Monad m       => m a -> m b -> m b
      ```

## Examples of Monad use

### List
* Binding in list
    * ```
        (>>=) :: Monad m => m  a -> (a ->  m  b) ->  m  b
        (>>=) ::           [ ] a -> (a -> [ ] b) -> [ ] b
        
        return :: Monad m => a ->  m  a
        return ::            a -> [ ] a
        
        (>>=) :: [a]  -> (a -> [b])   -> [b]
        return :: a -> [a]
      ```

### Maybe
* Binding in `Maybe`
    * ```
        (>>=) :: Monad m => m a -> (a ->     m b) ->     m b
        (>>=) ::        Maybe a -> (a -> Maybe b) -> Maybe b

        return :: Monad m => a ->     m a
        return ::            a -> Maybe a
      ```

### Either
* Binding in `Either`
    * ```
        (>>=) :: Monad m => m a -> (a ->        m b) ->        m b
        (>>=) ::     Either e a -> (a -> Either e b) -> Either e b

        return :: Monad m => a ->        m a
        return ::            a -> Either e a
      ```

## Monad laws

### Identity
* Right identity
    * `m >>= return = m`
* Left identity
    * `return x >>= f x`
* Both laws say that `return` should be neutral and not perform any
  computation.

### Associativity
* Regrouping the functions should not have any impact on the final result
    * `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

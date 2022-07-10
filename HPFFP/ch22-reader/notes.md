# Chapter 22 - Reader

Reader is a way of **stringing functions together** when all those functions
are **awating for one input** from a shared environment.

## This is Reader

* Functions have **Functor, Applicative, and Monad instances**.
    * The term **Reader** usually refers to Monad or Applicative instances
* Functor of functions is **function composition**.
    * Mapping an ordinary function over another to creater a new function
      awaiting a final argument (just like function composition).
* Applicative and Monad instances: give us a way to **map a function that is
  awaiting an argument** over **another function that is also awaiting an
argument**.

## Functor of function

```
    -- since (->) takes two args, type ((->) r) is waiting for one more arg.
    -- r is called reader, and it can be anything.
    instance Functor ((->) r) where
        fmap = (.)
```
* `((->) a) b` = `(->) a b` = `a (->) b`

## `Reader` newtype

* `Reader` is a newtype wrapper for the function type
    * ```
        newtype Reader r a = 
            Reader { runReader :: r -> a }
      ```
    * `r` is the type we are reading in, `a` is the result of the function.
* ```
    instance Functor (Reader r) where
        fmap :: (a -> b) -> Reader r a -> Reader r b
        fmap f (Reader ra) =
            Reader $ \r -> f (ra r)

    -- or, with function composition
    instance Functor (Reader r) where
        fmap :: (a -> b) -> Reader r a -> Reader r b
        fmap f (Reader ra) =
            Reader $ (f . ra)
  ```
* `ra` is the function wrapped in `Reader`

## Applicative of functions

* `f` in `Applicative f` is equivalent to `(->) r` or `r ->`
    * ```
        pure :: a ->     f a
        pure :: a -> (r -> a)

        (<*>) ::    f (a -> b) ->     f a ->      f b
        (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
      ```

## Monad of functions

```
    (>>=) :: Monad m =>     m a -> (a ->     m b) ->      m b
    (>>=) ::           (->) r a -> (a -> (->) r b -> (->) r b
    (>>=) ::           (r -> a) -> (a -> r -> b) -> (r -> b) 

    return :: Monad m => a ->      m a
    return ::            a -> (->) r a
    return ::            a ->   r -> a
```

## ReaderT is more prevalent than Reader

Reader usually comes with a Monad and become **transformers** (`ReaderT`).

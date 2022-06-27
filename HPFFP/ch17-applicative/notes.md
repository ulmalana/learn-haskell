# Chapter 17 - Applicative

* Previous two algebras:
    * Monoid: mashing two values of the same type together.
    * Functor: applying a function over some structure.
* Applicative is **Monoidal Functor**.
    * allowing for function application lifted over structure (like Functor)
    * the function we are applying is embedded in some structure.
    * both function and values are inside some structure, we can smash those
      together.

## Defining Applicative

* Typeclass declaration
    * ```
        class Functor f => Applicative f where
            pure :: a -> f a
            (<*>) :: f (a -> b) -> f a -> f b
      ```
* the difference between `(<\*>)` and `fmap`/`(<$>)`
    * ```
        (<$>) :: Functor f =>       (a -> b) -> f a -> f b
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b
      ```
* Other core functions
    * ```
        -- lifts function below are just fmap but with Applicative constraint
        liftA :: Applicative f => (a -> b) -> f a -> f b
        liftA2 :: Applicative f => (a -> b -> c ) -> f a -> f b -> f c
        liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
      ```
## Functor vs. Applicative

* Applicative is **Monoidal Functor**.
    * We need to embed function in a structure, then apply the embedded
      function over some structure
* In comparison to Functor:
    * `fmap f x = pure f <\*> x`
    * `fmap (+1) [1..5] == pure (+1) <\*> [1..5]`
* `pure` is for embedding a value of any type in the structure
    * ```
        > pure 1 :: [Int]
        [1]
        > pure 1 :: Maybe Int
        Just 1
        > pure 1 :: Either a Int
        Right 1
        > pure 1 :: ([a], Int)
        ([], 1)
      ```
## Applicative functors are monoidal functors

* Comparison:
    * ```
        ($)    ::   (a -> b) ->   a ->   b
        (<$>)  ::   (a -> b) -> f a -> f b
        (<\*>) :: f (a -> b) -> f a -> f b
      ```
* Monoid + function application
    * ```
        mappend :: Monoid a => a -> a -> a
        
        mappend ::    f           f      f
        $       :: (a -> b)       a      b
        
        (<\*>)  :: f (a -> b) -> f a -> f b
      ```

* Lifted function to list
    * ```
        -- function (*2) and (*3) are lifted to a list
        [(*2), (*3)] <*> [4,5]
        
        =
        
        [2*4, 2*5, 3*4, 3*5] = [8,10,12,15]
      ```
### Where is the monoid

* Tuple shows how monoid works in Applicative
    * `(a, b)` -> `a` needs to be a Monoid, while `b` doesnt since we use
      function application to produce `b`.
    * ```
        > :info (,)
        data (,) a b = (,) a b
        ...
        instance Monoid a => Applicative ((,) a)
        instance (Monoid a, Monoid b) => Monoid (a, b)
      ```
* Example: We can **apply lifted function to `b`** and use **`mappend` to `a`**
    * ```
        > ("Woo", (+1)) <*> (" Hoo!", 0)
        ("Woo Hoo!", 1)
        > ((Sum 2), (+1)) <*> ((Sum 0), 0)
        (Sum {getSum = 2}, 1)
        > ((Product 4), (+6)) <*> ((Product 2), 4)
        (Product {getProduct = 8}, 10)
      ```

## Applicative in use

### List Applicative

* ```
    (<*>) ::  f (a -> b) ->  f  a ->  f  b    -- default definition
    (<*>) :: [ ] (a -> b) -> [ ] a -> [ ] b   -- for list
  ```
* With list applicative, we are mapping **a plurality of functions** over **a
  plurality of values**.
    * ```
        > [(+1), (*2)] <*> [2,4]
        [3,5,4,8]
      ```
* In some sense, it is similar to **cartesian product**
    * ```
        > (,) <$> [1,2] <*> [3,4]
        [(1,3), (1,4), (2,3), (2,4)]
      ```
* When combining `fmap` with `ap`, first evaluate the `fmap`
    * ```
        (+) <$> [1,2] <*> [3,5] == ((+) <$> [1,2]) <*> [3,5]
        
        [(+1), (+2)] <\*> [3,5]

        [4,6,5,7]
      ```
    * Or we can use `liftA2`: `liftA2 (+) [1, 2] [3, 5]`

### Identity

* ```
    (<*>) ::        f (a -> b) ->        f a ->        f b
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  ```

* Having this extra bit of structure around our values lifts the `const` from
  mapping over the list to mappinf over Identity
    * ```
        > const <$> [1,2,3] <*> [9,9,9]
        [1,1,1,2,2,2,3,3,3]
        > const <$> Identity [1,2,3] <*> Identity [9,9,9]
        Identity [1,2,3]
      ```

### Constant

* **This maybe different in newer GHC version**

* ```
    (<*>) ::          f (a -> b) ->          f a ->          f b
    (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b
  ```
* This is similar to `const`, takes two args but discards one.
    * cant do anything because it can only hold onto one value
    * ```
        > Constant (Sum 1) <*> Constant (Sum 2)
        Constant {getConstant = Sum {getSum = 3}}
      ```

### Maybe Applicative

* ```
    (<*>) ::     f (a -> b) ->     f a ->     f b
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  ```



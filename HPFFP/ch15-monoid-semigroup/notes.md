# Chapter 15 - Monoids and Semigroup

* **Algebra**: 
    * study of math symbols and the rules governing it, or
    * **operations** and the **set** they operate over.

## Monoid

* **A binary associative operation with an identity**
    * **binary**: two of something
    * **associative**: property that must be satisfied.
    * **operation**: function. in this case function of two arguments.
    * **identity**: a value when combined with other value will **return that other value**.
    * `mappend x mempty = x`
    * `mappend mempty x = x`
* **In plain english**: **Monoid** is a **function** that takes **2 arguments**
  and follow 2 laws, **associativity and identity**.
    * Although it follows law of associativity, **not all Monoid** follow law
      of **commutativity**.
* `Monoid` typeclass
    * ```
        class Monoid m where
            mempty :: m
            mappend :: m -> m -> m
            mconcat :: [m] -> m
            mconcat = foldr mappend mempty
      ```
### Monoid example
* List
    * ```
        > mappend [1,2,3] [4,5,6]
        [1,2,3,4,5,6]
        > mconcat [[1..3], [4..6]]
        [1,2,3,4,5,6]
        > mappend "riz " "maulana"
        "riz maulana"
      ```
### No Monoid for numeric types

* **`Integer` and other numeric types** have no `Monoid` instance.
    * Not clear whether joining two number should be **summation or
      multiplication**.
    * Both operations are monoidal (binary, associative, and have identity) but
      each type should only have unique typeclass instance (to avoid
ambiguity).
    * Example: how should we proceed with `mappend 1 1`? sum or multiply?
* To resolve the issue, Haskell provides two newtypes `Sum` and `Product` to wrap numeric values and choose which monoid we want.
    * Example
    * ```
        > mappend (Sum 2) (Sum 3)
        Sum {getSum = 5}
        > mappend (Product 4) (Product 5)
        Product {getProduct = 20}
        > mappend (Sum 4.3) (Sum 1.2)
        Sum {getSum = 5.5}
      ```
* **Integer form a monoid under summation and multiplication**
* **Lists form a monoid under concatenation**
* Some types may have **more than one possible monoid**, **including List**.
* In `Data.Monoid`, there is the infix version of `mappend`: `(<>)`
    * When we want to join more than 2 values, we need to nest them:
    * `mappend (Sum 1) (mappend (Sum 2) (Sum 3))`
    * But with `(<>)`, we can easily write:
    * `(Sum 1) <> (Sum 2) <> (Sum 3)` -> `Sum {getSum = 6}`
    * We can also put them in a list and use `mconcat`:
    * `mconcat [(Product 2), (Product 3), (Product 4)]` -> `Product {getProduct 24}`

## Laws

* **Left identity**
    * `mappend mempty x = x`
* **Right identity**
    * `mappend x mempty = x`
* **Associativity**
    * `mappend x (mappend y z) = mappend (mappend x y) z`

* `mconcat = foldr mappend mempty`

* All the laws above can be guaranteed even when we dont know *what monoid*
  we'll be working with.

## Different typeclass instance, same representation

* **Mappending** is perhaps best thought of **not as a way of combining values** like addition or concatenation does, but as a way **to condense any set of values to a summary value**.

* **Example**: Boolean has two possible monoids:
    * conjunction (`All` newtype)
    * `All True <> All True` -> `All {getAll = True}`
    * disjunction (`Any` newtype)
    * `Any True <> Any True` -> `Any {getAny = True}`  
* Mappending is less about combining and more about **condensing or reducing**
* `Maybe` type has more than two possible Monoids. Example:
    * `First`: return the first/leftmost non-Nothing value
    * ``First (Just 1) `mappend` First (Just 2)`` -> `First {getFirst = Just
      1}`
    * `Last`: returns the last/rightmost non-Nothing value
    * ``Last (Just 1) `mappend` Last (Just 2)`` -> `Last {getLast = Just 2}`

## Orphan instance

* Orphan instance is a typeclass instance for type T which is not defined in
  the module where type T is defined.
* **AVOID THIS AT ALL COST**
* It can mess up with the behaviour of our programs when there are multiple
  orphan instances.

## Semigroup

* **Semigroup** is a Monoid **without an identity**.
    * the core operation: **binary and associative**
* `Semigroup` class
    * ```
        class Semigroup a where
            (<>) :: a -> a -> a
      ```
    * the only law left: `(a <> b) <> c = a <> (b <> c)`
* Because it has no identity, it is a **weaker algebra**.

### `NonEmpty` type
* `NonEmpty` list type: `Semigroup` but not `Monoid`.
    * a list that can never be empty
    * ```
        data NonEmpty a = a :| [a] deriving (Eq, Show, Ord)
      ``` 
* Useful for forcing a list that cant be empty.i

## Types vs operation

* There is an inverse relationship between the operations permitted over a type
  and the number of types that can satisfy.
* `id :: a -> a`
    * Number of types: Infinite
    * Number of operation: One
* `inc :: Num a => a -> a`
    * Number of types: anything that implements `Num`
    * Number of operations: 7 methods in `Num`
* `OnlyInt :: Int -> Int`
    * Number of types: One (`Int`)
    * Number of operations: more than 7.

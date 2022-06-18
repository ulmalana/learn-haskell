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
    * `mconcat [(Product 2), (Product 3), (Product 4)]` -> Product {getProduct
      24}


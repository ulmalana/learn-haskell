# Chapter 06 - Typeclasses

Typeclasses allow us **to generalize over a set of types** in order to **define
and execute a standard set of features** for those types.

Typeclasses have hierarchy. Example (from superset): `Eq` -> `Ord` -> `Enum`.
Meaning that to make an enumerated list, we need to be able to order something.
To order something, we need to be able to compare for equality.

## `Eq` Typeclass

* This typeclass is for comparing equality.
* It has two basic functions:
    * `(==) :: a -> a -> Bool`
    * `(/=) :: a -> a -> Bool`

## `Num` Typeclass

* This typeclass is implemented by most numeric types.
* There are several basic functions in `Num`:
    * `(+) :: a -> a -> a`
    * `(*) :: a -> a -> a`
    * `(-) :: a -> a -> a`
    * `negate :: a -> a`
    * `abs :: a -> a`
    * `signum :: a -> a`
    * `fromInteger :: Integer -> a`

There are also "sub-typeclass" of `Num` which also implemented in most numeric
types:

### `Integral`

* This is for integral numbers. It is defined as:
`class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer`

### `Fractional`

* This typeclass is for fractional numbers
`class (Num a) => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a`

## Type-defaulting typeclasses

Some typeclasses have the default concrete type. Here is the list specified by
Haskell Report:
    * `Num` -> `Integer`
    * `Real` -> `Integer`
    * `Enum` -> `Integer`
    * `Integral` -> `Integer`
    * `Fractional` -> `Double`
    * `RealFrac` -> `Double`
    * `Floating` -> `Double`
    * `RealFloat` -> `Double`

**Types can be made more specific, but not more general/polymorphic**

## `Ord` Typeclass

* This typeclass is for things that can be put in order.
* It has several basic functions:
    * `compare :: a -> a -> Ordering`
    * `(<) :: a -> a -> Bool`
    * `(<=) :: a -> a -> Bool`
    * `(>) :: a -> a -> Bool`
    * `(>=) :: a -> a -> Bool`
    * `max :: a -> a -> a`
    * `min :: a -> a -> a`

## `Enum` Typeclass

* This typeclass is for things that are enumerable (its predecessors and
  successors are known).
* Its basic functions:
    * `succ :: a -> a`
    * `pred :: a -> a`
    * `toEnum :: Int -> a`
    * `fromEnum :: a -> Int`
    * `enumFrom :: a -> [a]`
    * `enumFromThen :: a -> a -> [a]`
    * `enumFromTo :: a -> a -> [a]`
    * `enumFromThenTo :: a -> a -> [a]`
* Examples:
    * `enumFromTo 3 8` -> `[3,4,5,6,7,8]`
    * `enumFromTo 'a' 'f'` -> `"abcdef"`

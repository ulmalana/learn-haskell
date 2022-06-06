# Chapter 11 - Algebraic Datatypes

**A type** can be thought of as **an enumeration of constructors** that have
zero or more arguments. For example, `Bool` datatype is an enumeration of two
possible constructors (`False` and `True`) each which takes zero argument.

* creating our own types allow us to **model** our problem before thinking about **how to create computations** to solve the problem.

## Type dan Data constructors

* Constructors that takes no arguments are sometimes called **constants**
* Example `Bool`: 
    * `data Bool = False | True`
    * `Bool`: **type constructor** or **type constant**
    * `True` and `False`: data constructors
* Constructors that **take no argument behave like *constants***
* Constructors that **take arguments behave like functions** that dont do anything,
  except get applied.

## Kinds

* Kinds are **the types of types**.
* fully applied, concrete type (`Int`, `Bool, etc) is represented as **\***.
* parametric types that are waiting to be applied is represented as **\* -> \***. after applied, this type can become a concrete type and has kind **\***.

## Data constructors and values

This snippet ilustrates the distinction between data constructors and constant
values:

```
data PugType = PugData
--      [1]     [2]

data HuskyType a = HuskyData
--      [3]           [4]

data DogeType doge = DogeData doge
--      [5]           [6]
```

1. `PugType`: type constructor/type constant
2. `PugData`: **data constructor and constant value**.
3. `HuskyType`: polymorphic type constructor
4. `HuskyData`: **data constructor and constant value**. parameter `a` is   a
   **phantom** since it **has no witness** in the definition.
5. `DogeType`: polymorphic type constructor
6. `DogeData`: polymorphic data constructor and **not constant**.

## Type vs Data

* Types: **static**, resolve at **compile time**, dont persist through to runtime
* Data: used in **runtime**

## Arity

* **Arity**: **number of arguments** a function can take.

* **nullary**: takes zero argument
    * ```
        data Example0 = Example0 deriving (Eq, Show)
      ```
* **unary**: takes one argument
    * ```
        data Example1 = Example1 Int deriving (Eq, Show)
      ```
* **products**: data constructors that take more than one argument
    * ```
        data Example2 = Example2 Int String deriving (Eq, Show)
      ```

* **anonymous products**: tuples, which can take several args

## What makes algebraic

Algebraic datatypes are algebraic because **we can describe the patterns of
argument structures using *sum* and *product***.

* The cardinality of datatype is **the number of possible values it defines**
    * can be 1 to infinity
    * `Bool` has 2 constructors or values, so **its cardinality is 2**.
    * `Int8` has value from -128 to 127, so **its cardinality is 256**.
* `Example` type below only has one nullary constructor, which can be thought
  as having **one value**. So the cardinality of `Example` is **1**.
    * ```
        data Example = MakeExample deriving Show
      ```

* The cardinality of type with unary constructor **follows the cardinality of its
  type argument**.
    * Example: the cardinality `Goats` below follows the cardinality of `Int`.
      (In terms of cardinality, unary constructor becomes **identity function**).
    * `data Goats = Goats Int deriving (Show, Eq)`

### `newtype`

* Special keyword to define a type that can only have **a single unary data
  constructor**.
* The cardinality of `newtype` is the same as the cardinality of its type
  argument.
* `newtype` cant be a product type, sum type, or contain nullary constructors.
* `newtype` has no runtime overhead because it behaves like a `type` synonym
  and uses the representation of its type argument.
    * similar to `String` and `[Char]`
* However, we can define typeclass instances for `newtype` that differ from the
  instances for their underlying type.
    * ```
        -- Goats = Int
        newtype Goats = Goats Int deriving (Eq, Show)

        class TooMany a where
            tooMany :: a -> Bool

        -- instances for Int
        instance TooMany Int where
            tooMany n = n > 30

        -- instances for Goats. It can have the same instance as Int,
        -- but we want a different one
        instance TooMany Goats where
            tooMany (Goats n) = n > 35
      ```
* `newtype` can use typeclass instances from the contained types that is common
  (`Eq`, `Ord, `Enum`, `Show`) automatically.
* We need to use pragma `GeneralizedNewtypeDeriving` to make user-defined
  typeclass instances for `newtype`.

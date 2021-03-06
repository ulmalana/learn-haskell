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

## Sum types

* Example: `data Bool = False | True`
* `|` or pipe represents logical disjunction or **OR**. This is the **sum** in
  ADT.
* Get the cardinality of sum types by **summing** the cardinality of their
  constructors.
* `True` and `False` are nullary constructors and each have value of **1**
* By summing the cardinality of its constructors, `Bool` has cardinality of
  **2**.
* The cardinality of `BigSmall` is 4
    * ```
        data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
      ```

## Product types

* A data constructor with **two or more type arguments** is a **product**.
* Get the cardinality by **multplying** the cardinality of its constructors.
* Example:
    * ```
        -- QuantumBool is sum type and its cardinality is 3
        data QuantumBool = QuantumTrue
                        | QuantumFalse
                        | QuantumBoth deriving (Show, Eq)

        -- TwoQ is product type and its cardinality is
        -- QuantumBool x QuantumBool
        ---3 x 3 = 9
        data TwoQ = MkTwoQ QuantumBool QuantumBool deriving (Show, Eq)
      ```
* **IMPORTANT: cardinality of datatype ~= how difficult it is to reason** 

### Record syntax

* **Records**: product types with additional syntax to access the fields.
* Example:
    * ```
        -- simple product type
        data Person = MkPerson String Int deriving (Eq, Show)

        -- record
        data Person = 
            Person { name :: String,
                     age :: Int }
                   deriving (Eq, Show) 
      ```
* With record syntax, we can access the contained information with `name` and
  `age` functions.
    * ```
        > let rz = Person "Riz" 25
        > name rz
        "Riz"
      ```
## Normal form

* ADT also has **distributive property**
    * a \* (b + c) = (a \* b) + (a \* c)
* Normal form is **sum of products**
* Example of "sum of product" types, ie in normal form:
    * ```
        data Expr = Number Int
                    | Add Expr Expr
                    | Minus Expr
                    | Mult Expr
                    | Divide Expr Expr
      ```

## Function type is exponential

* **function type is the exponent operator**
    * a function `a -> b` have `b^a` inhabitants.
    * Example: `Bool -> Bool` have `2^2` inhabitants
* ```
    a -> b -> c
    (c ^ b) ^ a
    c ^ (b * a)
  ```
* example
    * ```
        data Quantum = Yes | No | Both deriving (Eq, Show)

        -- Quantum sum: 3 + 3
        quantSum1 :: Either Quantum Quantum 
        quantSum1 = Right Yes

        quantSum2 :: Either Quantum Quantum
        quantSum2 = Right No

        quantSum3 :: Either Quantum Quantum
        quantSum3 = Right Both
        
        quantSum4 :: Either Quantum Quantum
        quantSum4 = Left Yes
        .
        .
        .
      ```
    * ```
        -- Quantum product: 3 * 3
        quantProd1 :: (Quantum, Quantum)
        quantProd1 = (Yes, Yes)
        
        quantProd2 :: (Quantum, Quantum)
        quantProd2 :: (Yes, No)
        
        quantProd3 :: (Quantum, Quantum)
        quantProd3 = (Yes, Both)
        .
        .
        .
      ```
    * ```
        --- Quantum function: 3 ^ 3
        quantFlip1 :: Quantum -> Quantum
        quantFlip1 Yes = Yes
        quantFlip1 No = Yes
        quantFlip1 Both = Yes

        quantFlip2 :: Quantum -> Quantum
        quantFlip2 Yes = Yes
        quantFlip2 No = Yes
        quantFlip2 Both = No

        quantFlip3 :: Quantum -> Quantum
        quantFlip3 Yes = Yes
        quantFlip3 No = Yes
        quantFlip3 Both = Both
        .
        .
        .
      ```

## Higher-kinded datatypes

* Kind that needs to be supplied with arguments in order to become fully
  applied (**\***).
* Example:
    * **\* -> \***
    * **\* -> \* -> \***
* Getting comfortable with higher-kinded types is important as type arguments
  provide a generic way to express "holes" to be filled later.

## Lists are polymorphic

* List can contain values any type (that have **\*** kind).

### Infix type and data constructors

* **Non-alphanumeric operators** (+, \*, -, etc.) are **infix** by default.
* Any operator that starts with a colon (:) must be **an infix type or data
  constructor**.
* All infix data constructors must start with a colon.
    * the only exception: `(->)`
    * cant be `::` since it is reserved.
* Creating our own **non-alphanumeric data constructor**
    * ```
        data Product a b = a :&: b deriving (Eq, Show)
      ```
    * ```
        > 1 :&: 2
        1 :&: 2
        > :t 1 :&: 2
        1 :&: 2 :: (Num a, Num b) => Product a b
      ```
* Choosing to use infix data constructors is only aesthetic preference.

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

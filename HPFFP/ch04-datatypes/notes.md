# Chapter 04 - Datatypes

## Data Declaration
How datatypes are defined. Example:
`data Bool = False | True`
* `Bool` is **type constructor**
* `False` and `True` are **data constructor**

## Numeric types
There are several kinds of numeric types (all have typeclass `Num`):

### **Integral numbers** (whole numbers)
* `Int`: **fixed-precision integers**. Has max and min value.
* `Integer`: **arbitrarily large or small** integers. Should prefer `Integer`, not `Int`.

### Fractional
* `Float`: single-precision floating point. It is flexible (can be shifted and
  shoulb be used with great care). **Should not be used in business
applications**
* `Double`: double-precision floating point (twice as many bits as `Float`)
* `Rational`: fractional to represent ratio of two integers (ex: 1/2).
  **arbitrarily precise but not as efficient as `Scientific`**
* `Scientific`: space efficient and almost-arbitrary precision scientific
  number type in scientific notation. it stores the coefficient in `Integer`
and exponent in `Int`. **Comes from library, not included in GHC**

## Haskell's categories of entities
There are six categories of entities that have names:
* At term-level: **variables** and **data constructors**
* At type-level: **type variables**, **type construtors**, **typeclasses**
* **modules**

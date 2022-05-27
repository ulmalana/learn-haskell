# Chapter 05 - Types

* Expressions reduce to values, and **every value has a type**.
* **Types**: How we group a set of values that share something in common.
* A **good type system** should let us **only express programs that work** and
  express **many possible working programs**.
* **Static typing**: Types are known to the compiler and checked for mismatched
  at compile time.
## Querying types

When querying types with `:type` command in REPL, some values give some
specific type like `Bool`, `Char`, or `[Char]`, but other values (like
numerical value) will give the type with **broadest applicability (most polymorphic)**. 
For example, if we type `:type 13` we get `Num a => a` since it is the most
polymorphic and `13` can be used as `Integral` or `Fractional` number.
* **`(->)`** in function type is a **type constructor** that **takes arguments** and has **no data constructors**
    * `GHCi> :info (->)` returns `data (->) a b`
* Typeclass can be used to generalize some types so that they can be used in a
  broader way.

## Currying

All functions in Haskell **take one argument** and **return one argument**. The
return may be a value or a function waiting for the next argument.

### Uncurrying

We can "uncurry" a function by replacing the two functions with a tuple of two
values. Example: currying `(+)`, its type changes from `Num a => a -> a -> a`
to `Num a => (a, a) -> a` (**takes two arguments, return one result**).
* **Uncurried functions**: one function, many arguments.
* **Curried functions**: many functions, one argument apiece. 

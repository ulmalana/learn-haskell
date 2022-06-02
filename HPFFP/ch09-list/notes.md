# Chapter 09 - Lists

Two list purposes in Haskell:

* a way to **refer to and process a collection**
* as an **infinite series of values** to act as **a stream datatype**.

## List datatype

* `data [] a = [] | a : [a]`
* `(:)` or **cons** is to build the list recursively.

## Pattern matching on list

* `(x:xs)` where `x` is the first element, and `xs` is the rest of the list
* or `(x:y:xs)` where `x` is the first, `y` is the second, `xs` is the rest.

## Constructing lists

* Range
    * `[1..10]`
    * `enumFromTo 1 10`

* Range with skips
    * `[1,2..10]`
    * `enumFromThenTo 1 2 10]`

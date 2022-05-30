# Chapter 07 - Functions

* Functions are **applied** to their arguments.
* Arguments are **passed** functions, then evaluated.

## Arguments and Parameters

* A value that can be used as an argument to a function is a *first-class value*.
* Argument can be **shadowed** by another argument.
* Haskell is **lexically scoped**, meaning that resolving the value depends on
  the location in the code and the lexical context.

## Anonymous Functions

### Named functions

`triple :: Integer -> Integer
triple x = x * 3`

### Anonymous functions

`(\x -> x * 3) :: Integer -> Integer` 

## Pattern matching

* Incomplete pattern matches will return **bottom**, non-value used to denote
  that  the program cant return something. It will throw exception.


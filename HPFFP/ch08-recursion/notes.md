# Chapter 08 - Recursion

* **Defining a function in terms of itself.**

* Recursion is **a natural property of many logical and mathematical systems**,
like human language.

* It is also a means of expressing code that must take an **indefinite** number of steps to return a result.

* Recursive expressions need to have **a base case** in order to stop
* Recursion is also **self-referential compisition**.
* Haskell implemented recursion with **Y Combinator**.

## Bottom

* ⊥ or **bottom**: computations that dont successfully return result.
    * **failed with an error**
    * **failed to terminate**
* can be specified with `error` keyword to make a bottom in a code
    * example: `f True = error "stop here"`



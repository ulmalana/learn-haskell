# Chapter 01 - Lambda Calculus

* Lambda calculus devised by Alonzo Church in 1930s
* **Calculus**: a method of calculation or reasoning.
* **Lambda calculus**: a process for formalizing methods of effective
    computability (which problems can be solved).

## Functional programming

* Programming paradigm that relies on mathematical-modeled functions.
* Programs are combinations of **expressions**
* Expressions include *concrete values*, *variables*, and *functions*.
* Functions: expressions that are applied to arguments and can be evaluated
* Haskell is a pure functional language because anything can be translated into lambda calculus(?).
* Also, purity sometimes referes to **referential transparency**, meaning a
  function always returns the same values, just like in math.

## Structure of lambda calculus

* Lambda calculus has 3 components (lambda terms): **expressions**, **variables**, **abstractions**.
* Expressions: can be a variable name, an abstraction, or combination of both.
  Simplest expression: a single variable. Variable has no meanning or value, it is just a name.
* Abstraction is a function and has lambda (&lambda;) in it. An *argument*
  (input value) is passed to this function/abstraction.
* Abstraction consists of 2 parts: **head** and **body**.
    * head: &lambda; followed by a variable name
    * body: expression for this particular function
    * Example: &lambda;x.x
    * Parameter x (before the dot) in example above binds all x in the body
    * Example above also anonymous.
* **Alpha equivalence**: any variable in lambda is not semantically meaningful and
  can be replaced with other symbols. So, following functions are the same:
    * &lambda;x.x
    * &lambda;d.d
    * &lambda;z.z 
* **Beta reduction**: substituting the input expression for all instances/variables
  within the body, evaluate the expression, then remove the head of abstraction (&lambda;).
    * Example: applying **(&lambda;x.x) to 2** returns **2**
    * Another ex: **(&lambda;x.x+1) 2**  returns **3**
    * (&lambda;x.x)(&lambda;y.y)z -> (&lambda;y.y)z -> z
* **Free variables**: variables that are not bound in the head
    * (&lambda;x.xy) : x is bound variable, y is free variable
    * evaluating **(&lambda;x.xy)z** returns **zy**.
    * Alpha equivalence doesnt apply to free variables.
    * **(&lambda;x.xz)** != **(&lambda;x.xy)**
* **One &lambda;** only bind **one parameter** and accept **one argument**
    * functions with multiple args have multiple, nested heads.
    * evaluate the first (leftmost) head, and then the next head.
    * originally discovered by Moses Schonfinkel in 1920s
    * rediscovered by Haskell Curry and is commonly called *currying*
    * example: **(&lambda;xy.xy)** is a shorthand for two nested lambdas and
      becomes **&lambda;x(&lambda;y.xy)**

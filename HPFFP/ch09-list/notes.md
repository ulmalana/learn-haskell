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

## Extracting lists

* `take 3 [1..10]` -> `[1,2,3]`
* `drop 6 [1..10]` -> `[7,8,9,10]`
* `splitAt 5 [1..10]` -> `([1,2,3,4,5], [6,7,8,9,10])`
* `takeWhile (<8) (enumFromTo 5 15)` -> `[5,6,7]`
* `dropWhile (<8) [3..13]` -> `[8,9,10,11,12,13]`

## List comprehension

* `[x^2 | x <- [1..10], rem x 2 /= 0]`
* `[x^y | x <- [1..10], y <- [2,3], x^y < 200]`
* `acronym xs = [x | x <- xs, elem x ['A'..'Z']]` -> acronym generator

## Spines

* Lists in Haskell are a recursive series of **con cells `a : [a]`**
* **`(:)`** or the **con** is the spine that ties values.
* `[1,2,3]` is actually `1 : 2 : 3 : []` or `1 : (2 : (3 : []))`
* Above list can also be represented as a tree
    :
   / \
  1   :
     / \
    2   :
       / \
      3   []

* `3` is processed first with `[]`, up until putting `1` at the front.

## Non strict evaluation

* `:sprint` command in GHCi can be used to check whether some expressions has
  ben evaluated.

`> let blah = enumFromTo 'a' 'z'
 > :sprint blah
 blah = _`

`blah = _ ` indicates that `blah` is unevaluated. Then try to evaluate part of
`blah`:

`> take 1 blah
 > :sprint blah
 blah = 'a' : _
 > take 2 blah
 > :sprint blah
 blah = 'a' : 'b' : _`

After taking 2 elements of `blah`, `:sprint` command shows its two elements
(`'a'` and `'b'`) have been evaluated, and the rest is still unevaluated.

* Some commands are strict, so when running `:sprint` it may immediately display fully-evaluated expressions, like `length`. 

`> length blah
 26
 > :sprint blah
 blah = "abcdefghijklmnopqrstuvwxyz"`

### Spines are evaluated independently of values

* By default, expressions in Haskell are reduced to **Weak Head Normal Form
  (WHNF)**
* **Normal form** = fully evaluated
* **WHNF** = only evaluated **as far as is necessary to reach a data
  constructor**
* **WHNF** = normal form | evaluated until reaching a data constructor or
  lambda waiting arguments.
* Examples:
    * `(1, 2)` -> **WHNF & NF**
    * `(1, _ + _)` -> **WHNF, but not NF**, (+) can be evaluated
    * `(1, 1 + 1)` -> **WHNF, but not NF**, 1+1 can be evaluated
    * `\x -> x * 10` -> **WHNF & NF**, cant be reduced anymore without argument
    * `"Hehe" ++ "hoho"` -> **Neither WHNF nor NF**, in defined list, nothing left to evaluate
    * constructing list with ranges of function -> **WHNF, but not NF**
* Evaluation or consumption of the list **goes down the spine**.
* `length` is **spine strict** and **only evaluates the spines**, not the
  values in the list. So, `length [1, undefined, 2]` still returns `3` although
it contains `undefined` because `length` does not evaluate values, only spines.
* `sum` is example of forcing both the spine and values.
* `sum [] = 0
   sum (x:xs) = x + sum xs`

## Transforming list

We can apply a function to each element of a list recursively using:
    * `map :: (a -> b) -> [a] -> [b]` (specific for list)
    * `fmap :: Functor f => (a -> b) -> f a -> f b` (for Functor typeclass, including list)

* `map` **doesnt really traverse the whole list and apply the function**, only
  when we force it to.
* we can put anonymous function as function argument to `map` or `fmap`. 

## Filtering list

We can filter a list to include only elements we want with a predicate.

* filter only even number less than 10: `filter even [1..10]`

## Zipping list

* Combining values from multiple lists into a single list.
    * `> zip [1, 2] [4, 5, 6]` -> `[(1,2), (2,4)]`

* We can also unzip to recover the list with `unzip`. `unzip` may not recover the true lists since `zip` must stop on the shortest list.

* We can use `zipWith` to apply a function before zipping two lists.
    * `> zipWith (*) [1,2] [4,5,6]` -> `[4,10]`



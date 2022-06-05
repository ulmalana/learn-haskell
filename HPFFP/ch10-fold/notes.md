# Chapter 10 - Folding list

Folds ~ **catamorphism** ~ **a means of destructuring data**.
If the spine of a list is the structure of a list, then folding is what can
reduce that structure.

## Folding vs. Mapping

* `map :: (a -> b) -> [a] -> [b]`
    * `map (+1) 1 :      2 :      3 : []`
    * `    (+1) 1 : (+1) 2 : (+1) 3 : []`
* `foldr :: (a -> b -> b) -> b -> [a] -> b`
    * `foldr (+) 0 (1 :  2 :  3 : [])`
    * `             1 + (2 + (3 + 0))`

## Two stages of folding

* **Traversal**: recurses over the spined
* **Folding**: evaluation/reduction of the folding function applied to values

## Fold right

* `foldr` can be used with **lists that are potentially infinite**, because it may
  not evaluate some of the values and the spines in the list.
* The initial value of the list must be able to be evaluated (**not `undefined`**)

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)
```

`foldr (+) 0 [1,2,3]` -> `(+) 1 ((+) 2 ((+) 3 0))`

## Fold left

* `foldl` is **not good for very long list or even infinite list**.
* `foldl` evaluates the **whole spine before evaluating values**, accumulating
  bunch of unevaluated values and **may result in negative performance**.
* **Use `foldl'` instead**, that is stricter and forcing the evaluation of
  values.

```
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs
```

`foldl (+) 0 [1,2,3]` -> `((0 + 1) + 2) + 3`

## `scanr` & `scanl`

We can use **scans** functions to see how folds evaluate.

### `scanr`

```
> foldr (+) 0 [1..5]
15
> scanr (+) 0 [1..5]
[15,14,12,9,5,0]
```

### `scanl`

```
> foldl (+) 0 [1..5]
15
> scanl (+) 0 [1..5]
[0,1,3,6,10,15]
```

## Associativity

* `foldr` and `foldl` may **give different results** for the same arguments
    * ```
        foldr (^) 2 [1..3]
        (1 ^ (2 ^ (3 ^ 2)))
        (1 ^ (2 ^ 9))
        (1 ^ 512)
        1
      ```
    * ```
        foldl (^) 2 [1..3]
        ((2 ^ 1) ^ 2) ^ 3
        (2 ^ 2) ^ 3
        4 ^ 3
        64
      ```

### Folding with cons

In the example below, with `foldl` we need to `flip` the cons `(:)` because it
has `a -> [a]`, which is not compatible with the associativity `foldl` thus
needs to be flipped to `[a] -> a`. The flipped result is caused by the
left-associativity of `foldl`.

```
> foldr (:) [] [1..3]
[1,2,3]
> foldl (flip (:)) [] [1..3]
[3,2,1]
```

## How to write fold functions

1. Think about the **starting value**
    * Usually the `identity` for the function, ex: `0` for sum, `1` for
      muliplication.

2. Consider the arguments
    * a folding function takes 2 args: `a` and `b`.
    * `a` is **one element of the list**
    * `b` is **starting value** or **accumulated value**

## `foldr` and `foldl` relationship

`foldr f acc xs` **=** `foldl (flip f) acc (reverse xs)`

## Scan function

* Similar to fold, but returns a list of intermediate values of evaluation

* ```
    foldr :: (a -> b -> b) -> b -> [a] -> b
    scanr :: (a -> b -> b) -> b -> [a] -> [b]

    foldl :: (b -> a -> b) -> b -> [a] -> b
    scanl :: (b -> a -> b) -> b -> [a] -> [b]
  ```

* Example:
    * ```
        scanr (+) 0 [1..3]
        [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
        [6, 5, 3, 0]
      ```
    * ```
        scanl (+) 0 [1..3]
        [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3]
        [0, 1, 3, 6]
      ```

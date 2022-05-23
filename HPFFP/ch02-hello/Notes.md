# Chapter 02 - Hello Haskell

## Installation
Install using [GHCup](https://www.haskell.org/ghcup).

## GHCi

Some commands:
    * `:quit` or `:q` : exit GHCi
    * `:info` or `:i` : get some info
    * `:load` or `:l` : load scriptsT

### Personal workflow
* `Prelude> :set prompt "> "`
* `> :set editor vim`
* `> :edit test.hs`
* `> :load test.hs`

## Expressions
* Expressions are in **normal form** when there are no more evaluation steps to
  be taken (reached *irreducible form*).
* Example : 1 + 1 is reducible expresions (**redex**), 2 is its normal form.

## Translating Let and Where expressions to Lambda

* `let a = b in c`
    * `(\a -> c) b`
* `let x = 10 in x + 9001`
    * `(\x -> x + 9001) 10`
* `c where a = b`
    * `(\a -> c) b`
* `x + 9001 where x = 10`
    * `(\x -> x + 9001) 10`

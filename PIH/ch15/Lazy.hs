-- Lazy Evaluation
--
-- function for incrementing integer
-- 
inc :: Int -> Int
inc n = n + 1
--
-- The expression inc (2*3) can be evaluated in two ways
-- First:
--      inc (2*3)   { applying * }
-- =    inc 6       { applying inc }
-- =    6 + 1       { applying + }
-- =    7
--
-- Second:
--      inc (2*3)   { applying inc }
-- =    (2*3) + 1   { applying * }
-- =    6 + 1       { applying + }
-- =    7
--
-- Both evaluation order does not affect the final result and important
-- general property of function application in Haskell.
-- In Haskell, any two DIFFERENT WAYS of evaluating the same expression
-- will produce the SAME FINAL VALUE, as long as they both terminate.
-- ** This property does not hold for most imperative programming **

-- Evaluation strategies
--
-- An expression that has the form of a function applied to one or
-- more arguments that can be "reduced" by performing the application
-- is called *reducible expression* or *redex*.
-- Ex: multiplication function
mult :: (Int, Int) -> Int
mult (x,y) = x * y

-- The expression mult (1+2, 2+3) has three redex
-- 1. 1+2               -> mult (3, 2+3)
-- 2. 2+3               -> mult (1+2, 5)
-- 3. mult (1+2, 2+3)   -> (1+2) * (2+3)

-- Innermost evaluation (innermost and leftmost first)
-- (arguments are passed by value)
-- Ex: 
--      mult (1+2, 2+3) { applying the first + }
-- =    mult (3, 2+3)   { applying + }
-- =    mult (3, 5)     { applying mult }
-- =    3 * 5           { applying * }
-- =    15

-- Outermost evaluation (outermost and leftmost first)
-- (arguments are passed by name)
-- Ex:
--      mult (1+2, 2+3) { applying mult }
-- =    (1+2) * (2+3)   { applying the first + }
-- =    3 * (2+3)       { applying + }
-- =    3 * 5           { applying * }
-- =    15


-- Curried version of mult
mult' :: Int -> Int -> Int
mult' x = \y -> x * y

-- Then using innermost evaluation
--      mult' (1+2) (2+3)   { applying the first + }
-- =    mult' 3 (2+3)       { applying mult }
-- =    (\y -> 3 * y) (2+3) { applying + }
-- =    (\y -> 3 * y) 5     { applying the lambda }
-- =    3 * 5               { applying * }
-- =    15

-- Infinite increment (does not terminate)
inf :: Int
inf = 1 + inf
-- How it works
--      inf                 { applying inf }
-- =    1 + inf             { applying inf }
-- =    1 + (1 + inf)       { applying inf }
-- =    1 + (1 + (1 + inf)) { applying inf }
--      .
--      .
--      .

-- consider the expression fst (0, inf)
-- with call-by-value (innermost) evaluation *does not terminate*
--      fst (0, inf)            { applying inf }
-- =    fst (0, 1 + inf )       { applying inf }
-- =    fst (0, 1 + (1 + inf))  { applying inf }
--      .
--      .
--      .
-- with call-by-name (outermost) evaluation
-- only takes 1 step and immediately stop
--      fst (0, inf) { applying fst }
-- =    0

-- ** important property **
-- if there exists any evaluation sequence that terminates for 
-- a given expression, then call-by-name evaluation will 
-- also terminate for this expression with the same final result.

-- ** Call-by-name evaluation is preferable to call-by-value for
-- ensuring that evaluation terminates.

-- square function
square :: Int -> Int
square n = n * n
-- evaluate with call-by-value
--      square (1+2)    { applying + }
-- =    square 3        { applying square }
-- =    3 * 3           { applying * }
-- =    9
--
-- evaluate with call-by-name
-- (this will need one extra steps for the duplicated 1+2 expression)
--      square (1+2)    { applying square }
-- =    (1+2) * (1+2)   { applying the first + }
-- =    3 * (1+2)       { applying + }
-- =    3 * 3           { applying * }
-- =    9

-- Call-by-value evaluates precisely once,
-- call-by-name may evaluates many times.

-- Call-by-name inefficiency can by solved using pointers
-- to indicate sharing the same expressions during evaluation.
-- example:
--  square (1+2) { applying square }
-- =  & * & ----> 1+2 (keep one copy argument, make 2 pointers) { applying + }
-- =  & * & ----> 3 { applying * }
-- = 9

-- The combination of call-by-name evaluation with sharing pointers is
-- known as **lazy evaluation**.

-- Lazy evaluation ensures evaluation terminates as often as possible
-- and never requires more steps than call-by-value.

-- Lazy evaluation allows infinite structures
ones :: [Int]
ones = 1 : ones
-- evaluating the expression head ones
-- with call-by-value
--      head ones               { applying ones }
-- =    head (1 : ones)         { applying ones }
-- =    head (1 : (1: ones))    { applying ones }
--      .
--      .
--      .
-- with call-by-name (without sharing)
--      head ones       { applying ones }
-- =    head (1 : ones) { applying head }
-- =    1

-- Using lazy evaluation, expressions are only evaluated as much as
-- required by the context in which they are used.

-- With lazy evaluation, ones above is not an infinite list,
-- but rather a potentially infinite list.

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
--

-- Equational Reasoning
double :: Int -> Int
double x = x + x
-- the expression double x can be replaced by x + x
-- conversely, x + x can be replaced by double x.
-- the function definition are equal in both ways.
-- but not all functions have this property. Ex:
isZero :: Int -> Bool
isZero 0 = True
isZero n = False
-- The first equation isZero 0 = True is "equivalent" in both directions,
-- but the second equation is not, caused by the order of evaluation
-- (first equation then the second). So, isZero n can only be replaced
-- by False if n =! 0. Dually, False can be replaced by isZero n when 
-- n != 0.

-- Functions with multiple equation may not follow this equational
-- property since each equation will be evaluated in order (ex: 
-- pattern matching), so a care is needed.

-- It is preferable to define functions in a manner that does not rely
-- on the order in which their equations are written. 
-- isZero function can be rewritten to follow this property:
isZero' 0           = True
isZero' n | n /= 0  = False
-- Now, isZero n can only be replaced by False, and conversely
-- False can only be replaced by isZero n when n =! 0.

-- Patterns that dont rely on the order in which they are matched
-- are called **non-overlapping**

-- Induction
-- when the base case is true, then the recursive case is also true
-- (imagine domino effect)
data Nat = Zero | Succ Nat
-- If some property p holds for Zero and for Succ, then it holds for any
-- number of n Succ.

-- adding two natural numbers
add :: Nat -> Nat -> Nat
add Zero m      = m
add (Succ n) m  = Succ (add n m)

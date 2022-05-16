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

-- Removing append for efficiency
-- consider the following reverse function
reverse16 :: [a] -> [a]
reverse16 []        = []
reverser16 (x:xs)   = reverse16 xs ++ [x]
-- ++ takes linear time of its first argument
-- also reverse16 function has a quadratic time (n^2)

-- auxiliary reverse function without ++
-- (consult the book why this function came up)
reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x : ys)

-- redefine reverse16 with auxiliary function above
reverse16' :: [a] -> [a]
reverse16' xs = reverse' xs [] 
-- Example
--      reverse16' [1,2,3]          { applying reverse16' }
-- =    reverse' [1,2,3] []         { applying reverse' }
-- =    reverse' [2,3] (1:[])       { applying reverse' }
-- =    reverse' [3] (2:(1:[]))     { applying reverse' }
-- =    reverse' [] (3:(2:(1:[])))  { applying reverse' }
-- =    3:(2:(1:[]))
--
-- with this techniques, reverse16' has linear time
-- compared to the original reverse16 which has quadratic time

-- Another example of removing append in binary tree
data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten (Leaf n)    = [n]
flatten (Node l r)  = flatten l ++ flatten r

-- with the same techniques as above, create an auxiliary function
-- (also consult the book)
flattenAux :: Tree -> [Int] -> [Int]
flattenAux (Leaf n) ns  = n : ns
flattenAux (Node l r) ns = flattenAux l (flattenAux r ns)

-- with flattenAux above, redefine flatten without append
flatten' :: Tree -> [Int]
flatten' t = flattenAux t []

-- Compiler
-- (also shows up in chapter 8)
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- evaluating Expr indirectly using stack
data Op = PUSH Int | ADD deriving Show

type Stack = [Int]
type Code = [Op]

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n+m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- Example: (2+3)+4
-- > let e = Add (Add (Val 2) (Val 3)) (Val 4)
--
-- > eval e
-- 9
--
-- > comp e
-- [PUSH 2, PUSH 3, ADD, PUSH 4, ADD]
--
-- > exec (comp e) []
-- [9]
--
-- In this case
-- exec (comp e) [] = [eval e]
-- or
-- exec (comp e) s = eval e : s

-- Since comp function includes appends, we can redefine it without ++
-- first, create an auxiliary function (consult the book)
compAux :: Expr -> Code -> Code
compAux (Val n) c = PUSH n : c
compAux (Add x y) c = compAux x (compAux y (ADD : c))
-- then we can define comp to be
comp' e = compAux e []
-- The correctness of the new compiler becomes
--  exec (compAux e c) s = exec c (eval e : s)

-- Calculating Compilers

-- basic building blocks for arithmetic expressions
data Expr = Val Int | Add Expr Expr

eval1 :: Expr -> Int
eval1 (Val n) = n
eval1 (Add x y) = eval1 x + eval1 y

-- 1 of 3 transformation steps
-- adding a stack
-- (stack make the argument manipulation explicit)
type Stack = [Int]

-- auxiliary eval function
eval' :: Expr -> Stack -> Stack
eval' (Val n) s = push n s
eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n+m : s

eval2 :: Expr -> Int
eval2 e = head (eval' e [])

-- example: 1+2
--      eval2 (Add (Val 1) (Val 2)) { applying eval2 }
-- =    head (eval' (Add (Val 1) (Val 2)) []) { applying eval' }
-- =    head (add (eval' (Val 2) (eval' (Val 1) []))) { applying inner eval' }
-- =    head (add (eval' (Val 2) (push 1 []))) { applying eval' }
-- =    head (add (push 2 (push 1 []))) { applying push }
-- =    head (add (2:1:[]))     { applying add }
-- =    head (3:[])             { applying head }
-- =    3

-- 2 of 3 transformation steps
-- adding a continuation
type Cont = Stack -> Stack

-- auxiliary eval function
eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

eval3 :: Expr -> Cont
eval3 e s = eval'' e id s

module FunctionWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

-- let expression with lambda
printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

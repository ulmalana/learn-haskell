-- Normal form
-- Normal form is sum of products
-- a * (b + c) = (a * b) + (a * c)

-- The following code is not in normal form 
-- because Author uses tuple
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- The following code is in normal form
-- because it is a sum of products type.
-- products type = Fiction2 AuthorName and Nonfiction2 AuthorName
-- sum type = combine product types above with |
data Author2 = Fiction2 AuthorName | Nonfiction2 AuthorName deriving (Show, Eq)

-- Another example of sum of products type (normal form)
data Expr = Number Int
            | Add Expr Expr
            | Minus Expr
            | Mult Expr Expr
            | Divide Expr Expr

-- Expr type that is not in normal form (sum of product)
type Number2 = Int
type Add2 = (Expr2, Expr2)
type Minus2 = Expr2
type Mult2 = (Expr2, Expr2)
type Divide2 = (Expr2, Expr2)

--type Expr2 = 
--    Either Number2 
--        (Either Add2
--            (Either Minus2
--                (Either Mult2 Divide2)))

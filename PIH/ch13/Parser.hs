-- Parser : a program that takes a string of char and returns a form of structural tree. 

-- Given a type Tree, a parser can be thought as a function
-- that takes a String and return a Tree
--
-- type Parser = String -> Tree

-- Sometimes parser not consume its entire argument string.
-- ex: parser for number might be applied to a string 
--     comprising a number followed by a word
--     like "123ab"
-- 
-- type Parser = String -> (Tree, String)

-- Parser might not always succedd
-- like a parser for numbers be applied to String.
-- For handling this error, a list of parsing results
-- can be returned instead.
--
-- type Parser = String -> [(Tree, String)]

-- Parser may return different kind of tree
-- ex: parser for numbers may return integer value
-- It is useful to parameterise the parser.
--
-- type Parser a = String -> [(a, String)]

-- Parser with type String -> [(a, String)] is similar to state
-- transformer with type State -> (a, State), so, parser can
-- be viewed as a generalised form of state transformer.

import Control.Applicative
import Data.Char

-- Px is a dummy type constructor
newtype Parser a = Px (String -> [(a, String)])

-- simply removes the Px dummy constructor for applying
-- the parser to an input string
parse :: Parser a -> String -> [(a, String)]
parse (Px p) inp = p inp

-- first parsing primitive
-- fails if the input is empty
-- otherwise, take the first character as the result
item :: Parser Char
item = Px (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

-- make Parser into a Functor
-- fmap applies function g to the result value of a parser if it succedds 
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = Px (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> [(g v, out)])


-- make Parser into an Applicative
instance Applicative Parser where
    -- pure converts a value to a Parser that always succeeds
    -- without consuming any of its input
    -- pure :: a -> Parser a
    pure v = Px (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Px (\inp -> case parse pg inp of
                            [] -> []
                            [(g, out)] -> parse (fmap g px) out)

-- This Parser consumes three characters, discards the second
-- and returns the first and third as a pair
three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

-- make Parser into Monad
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Px (\inp -> case parse p inp of
                            [] -> []
                            [(v, out)] -> parse (f v) out)

-- three function in Monad
three' :: Parser (Char, Char)
three' = do
            x <- item
            item
            z <- item
            return (x, z)

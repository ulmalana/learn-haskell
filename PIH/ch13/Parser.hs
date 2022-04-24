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

-- Making choices
-- combining and sequencing multiple parsers to process input.
-- if one parser fails, the next parser can be applied.
-- this can be done using Alternative typeclass in Control.Applicative
--
-- class Applicative f => Alternative f where
--      empty :: f a
--      (<|>) :: f a -> f a -> f a

-- and satisfy the following laws:
--          empty <|> x     = x
--          x <|> empty     = x
--          x <|> (y <|> z) = (x <|> y) <|> z

-- make Maybe into Alternative
data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap g (Just' x) = Just' (g x)

instance Applicative Maybe' where
    -- pure :: a -> Maybe' a
    pure = Just'

    -- (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    Nothing' <*> _ = Nothing'
    (Just' g) <*> mx = fmap g mx

instance Alternative Maybe' where
    -- empty :: Maybe a
    empty = Nothing'

    -- (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing' <|> my = my
    (Just' x) <|> _ = Just' x

-- make Parser into Alternative
instance Alternative Parser where
    -- empty :: Parser a
    empty = Px (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = Px (\inp -> case parse p inp of
                            [] -> parse q inp
                            [(v, out)] -> [(v, out)])

-- Derived primitives

-- parser for a single characters that satisfy predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do
        x <- item
        if p x 
        then return x
        else empty

-- using sat, define parsers for single digits, lowercase, uppercase,
-- arbritary letter, alphanumeric, and specific char
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

--

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                char x
                string xs
                return (x:xs)

-- both of these parsers have been defined in Alternative typeclass
--
-- many p: apply parser p multiple times until it fails, permits zero apply
-- some p: apply parser p multiple times untul it fails, and at least one of
--         them successfully applied

-- parser for identifier (variable name) comprising lowercase followed by
-- zero or more alphanum
varName :: Parser String
varName = do
          x <- lower
          xs <- many alphanum
          return (x:xs) 

-- parser for natural numbers with one or more digits
nat :: Parser Int
nat = do
        xs <- some digit
        return (read xs)

-- parser for space, tab, and newline char
space :: Parser ()
space = do
        many (sat isSpace)
        return ()

-- parser for integer
int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
      <|> nat

-- Handling spacing
-- ignore any space before and after applying a parser for a token
-- 1+2 is the same as 1 + 2
token :: Parser a -> Parser a
token p = do
            space
            v <- p
            space
            return v

-- using token function above, define parsers that ignore space around 
-- identifiers, natural numbers, integers, and special symbols
identifier :: Parser String
identifier = token varName

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- parser for non-empty natural number list which ignores space
nats :: Parser [Int]
nats = do
        symbol "["
        n <- natural
        ns <- many (do 
                    symbol "," 
                    natural)
        symbol "]"
        return (n:ns)

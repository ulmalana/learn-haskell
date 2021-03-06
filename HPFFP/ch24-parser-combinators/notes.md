# Chapter 24 - Parser Combinators

* **Parser** is a function that **takes some textual input** and **returns some
  structure as an output**.
* **Parser combinator** is a **higher-order function** that takes parsers as input
  and returns a new parser as output.

## Parser type

* Simple Parser type: `type Parser a = String -> Maybe (a, String)`
    * await a string value
    * produce a result which may fail
    * return a tuple of the value we wanted and whatever is left of the string
      that hasnt been consumed to produce a.

* Example:
    * parse character 1: `char '1'`
    * parse character 1 and 2: `char '1' >> char '2'`
    * parse 12 of 123: `parseString (char '1' >> char '1') mempty "123"`

## Alternative

We can parse a text that consist of combinations of many types (ex. string and
integer) with `Alternative` typeclass.

```
    class Applicative f => Alternative f where
        -- the identity of <|>
        empty :: f a
        -- associative binary operation (similar to OR)
        (<|>) :: f a -> f a -> f a

        -- one or more
        some :: f a -> f [a]
        some v = some_v
            where
                many_v = some_v <|> pure []
                some_v = (fmap (:) v) <*> many_v

        -- zero or more
        many :: f a -> f [a]
        many v = many_v
            where
                many_v = some_v <|> pure []
                some_v = (fmap (:) v) <*> many_v
```
## Character and token parsers

Traditionally, parsing has been done in two stages:
* Lexing (tokenization)
    Lexer will emit token to the parser until it has no more to emit. It is
more about **ignoring the noise and focus on the structures we are parsing**.

* Parsing
    Parser then structures the stream of tokens into a tree (abstract syntax
tree or AST).


## Marshalling from AST to datatype

* **Marshalling** is the process of preparind fata for serialization
* **Unmarshalling** is to get the data from a serialized one.

### How it works

**parse** -> **unmarshall**
`Text` -> `Structure` -> `Meaning`

**marshall** -> **serialize**
`Meaning` -> `Structure` -> `Text`


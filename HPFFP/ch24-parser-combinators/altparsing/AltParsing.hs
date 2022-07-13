{-# LANGUAGE QuasiQuotes #-}
module AltParsing where


import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah456"

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

eitherOr' :: String
eitherOr' = [r|
123
abc
456
def|]

-- parse integer or string in a text
parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

-- parseNos with skipping first char ("\n")
parseNos' :: Parser NumberOrString
parseNos' =
    skipMany (oneOf "\n")
    >>
    (Left <$> integer) <|> (Right <$> some letter)

-- always check if there is \n
parseNos'' :: Parser NumberOrString
parseNos'' = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

main = do
    print $ parseString (some letter) mempty a
    print $ parseString integer mempty b
    print $ parseString parseNos mempty a
    print $ parseString parseNos mempty b
    print $ parseString (many parseNos) mempty c
    print $ parseString (some parseNos) mempty c
    print $ parseString parseNos mempty eitherOr
    print $ parseString parseNos' mempty eitherOr
    print $ parseString (some parseNos') mempty eitherOr'
    print $ parseString (some parseNos'') mempty eitherOr

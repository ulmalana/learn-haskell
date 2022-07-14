{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseIni where


import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec


headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

-- parse the bracket pair then discard. finally return p inside the bracket.
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


commentEx :: ByteString
commentEx = "; this is a comment. 14/07/2022"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n;huhu"

skipComments :: Parser ()
skipComments =
    skipMany (do _ <- char ';' <|> char '#'
                 skipMany (noneOf "\n")
                 skipEOL)

sectionEx :: ByteString
sectionEx =
    "; ignore me\n[lang]\nHaskell=2020"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[lang]
Haskell=2020
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[newsect]
pi=3.14
|]

data Section = 
    Section Header Assignments
    deriving (Eq, Show)

newtype Config = 
    Config (Map Header Assignments)
    deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
    skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m =
    M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = 
            foldr rollup M.empty sections
    return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment
                    mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot" , "1")

    describe "Header Parsing" $ 
        it "can parse a simple header" $ do
            let m = parseByteString parseHeader mempty headerEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")

    describe "Comment Parsing" $
        it "can skip a comment before a header" $ do
            let p = skipComments >> parseHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")

    describe "Section Parsing" $
        it "can parse a simple section" $ do
            let m = parseByteString parseSection
                    mempty sectionEx
                r' = maybeSuccess m
                lang = M.fromList [("Haskell", "2020")]
                expected' = Just (Section
                                    (Header "lang")
                                    lang)
            print m
            r' `shouldBe` expected'

    describe "INI Parsing" $
        it "can parse multiple sections" $ do
            let m = parseByteString parseIni mempty sectionEx''
                r' = maybeSuccess m
                sectionValues = M.fromList
                                [ ("alias", "claw"),
                                  ("host", "wikipedia.org")]
                newsectValue = M.fromList [("pi", "3.14")]
                expected' = Just (Config
                                    (M.fromList 
                                        [ (Header "section", sectionValues),
                                          (Header "newsect", newsectValue)]))
            print m
            r' `shouldBe` expected'

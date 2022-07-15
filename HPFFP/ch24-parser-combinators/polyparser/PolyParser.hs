{-# LANGUAGE OverloadedStrings #-}

module PolyParser where


import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))
import Data.String (IsString)
import Data.Attoparsec.Text (parseOnly)

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, MonadFail m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "Denominator cant be zero"
      _ -> return (numerator % denominator)

main :: IO ()
main = do
    -- use attoparsec
    print $ parseOnly parseFraction badFraction
    print $ parseOnly parseFraction shouldWork
    print $ parseOnly parseFraction shouldAlsoWork
    print $ parseOnly parseFraction alsoBad

    -- use trifecta
    print $ parseString parseFraction mempty badFraction
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad

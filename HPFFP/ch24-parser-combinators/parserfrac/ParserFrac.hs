{-# LANGUAGE OverloadedStrings #-}

module ParserFrac where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldWorkToo = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

betterFrac :: Parser Rational
betterFrac = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cant be zero"
        _ -> return (numerator % denominator)

testBetterFrac :: IO ()
testBetterFrac = do
    print $ parseString betterFrac mempty badFraction
    print $ parseString betterFrac mempty alsoBad
    print $ parseString betterFrac mempty shouldWork
    print $ parseString betterFrac mempty shouldWorkToo


-- parsing badFraction first will stop this function because of exception.
-- testBetterFrac can handle that.
main :: IO ()
main = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldWorkToo
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction


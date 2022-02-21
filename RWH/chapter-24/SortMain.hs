module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)

import Sorting

testFunction = parSort
--testFunction = seqSort

randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g :: [Int])
                 in force result `seq` result

main = do 
    args <- getArgs
    let count | null args = 500000
              | otherwise = read (head args)
    input <- randomInts count   `fmap` getStdGen
    putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
    start <- getCurrentTime
    let sorted = testFunction input
    putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

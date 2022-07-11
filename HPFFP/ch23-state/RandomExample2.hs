module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix
            deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 number: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1,6)
    return (intToDie n, s)

-- lift rollDie
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

-- this function still gives us the same result.
-- run this function with initial state:
-- > evalState rollDieThreeTimes (mkStdGen 0)
rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

-- generate n random values with replicateM.
-- it will generate the same values from the same seed.
-- also run in REPL with seed:
-- > evalState (nDie 5) (mkStdGen 0)
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- run this function with:
-- > rollsToGetTwenty (mkStdGen 0)
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1,6) gen
                in go (sum + die) (count + 1) nextGen

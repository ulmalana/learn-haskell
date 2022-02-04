{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply (
    Supply,
    next,
    runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving (Functor, Applicative)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of 
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

unwrapS :: Supply s a -> State [s] a 
unwrapS (S s) = s

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return

showTwo :: (Show s) => Supply s String
showTwo = do 
    a <- next 
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)
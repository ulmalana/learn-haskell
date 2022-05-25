module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool
        then putStrLn "You are cool"
    else
        putStrLn "Not coolll"
    where cool = coolness == "cool"

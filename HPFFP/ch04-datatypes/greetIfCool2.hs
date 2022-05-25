module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
        then putStrLn "Hey you are cool"
    else
        putStrLn "Nonono"
    where cool v = v == "cool"

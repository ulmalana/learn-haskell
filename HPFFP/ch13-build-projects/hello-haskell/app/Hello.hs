module Hello where

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hallo, " ++ name ++ " ini dari module!")


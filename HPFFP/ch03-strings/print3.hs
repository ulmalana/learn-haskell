module Print3 where

myGreeting :: String
myGreeting = "henlo" ++ " oleh"

hello :: String
hello = "hello"

riz :: String
riz = "riz"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
        where secondGreeting = concat [hello, " ", riz]

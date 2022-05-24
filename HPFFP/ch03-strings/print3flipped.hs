module Print3Flipped where

myGreeting :: String
myGreeting = (++) "hello" " olleh"

hello :: String
hello = "hellon"

riz :: String
riz = "Riz"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
        where secondGreeting = (++) hello ((++) " " riz)

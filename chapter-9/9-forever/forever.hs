import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    input <- getLine
    putStrLn $ map toUpper input
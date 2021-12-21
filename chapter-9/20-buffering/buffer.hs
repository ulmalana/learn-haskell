import System.IO

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do 
        hSetBuffering handle $ BlockBuffering (Just 2048) -- set buffer 2048 B
        contents <- hGetContents handle
        putStr contents)
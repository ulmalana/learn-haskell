import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do 
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do 
                ineof <- hIsEOF inh
                if ineof
                    then return ()
                    else do inStr <- hGetLine inh
                            hPutStrLn outh (map toUpper inStr)
                            mainloop inh outh
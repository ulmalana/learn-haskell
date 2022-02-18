import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

main = do 
    args <- getArgs
    putStrLn $ "command line args: " ++ show args
    putStrLn $ "number of cores: " ++ show numCapabilities
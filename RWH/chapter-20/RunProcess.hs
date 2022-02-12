-- this is also the updated version of RunProcessSimple of the book
-- credit to Adam Yin who provided this version in the comment section
-- NOTE: this code still has error for piping
--       ex: 
--          > run $ "ls /etc" -|- grep "m.*ap" -|- "tr a-z A-Z" :: IO String
--            <interactive>: <file descriptor: 12>: commitBuffer: illegal operation (handle is closed)


{-# LANGUAGE FlexibleInstances #-}

module RunProcess where

import System.Process hiding (createPipe)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException, evaluate)
import System.Directory (setCurrentDirectory, createDirectory)
import System.IO
import System.Exit
import Text.Regex.Posix
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Data.List
import System.Posix.Env (getEnv)

import RunProcessSimple

instance CommandLike String where
    invoke cmd closefds input = do 
        esh <- getEnv "SHELL"
        let sh = maybe "/bin/sh" id esh
        invoke (sh, ["-c", cmd]) closefds input

instance CommandLike (String -> IO String) where
    invoke func _ input = return $ CommandResult (func input) (return $ Exited ExitSuccess)

instance CommandLike (String -> String) where
    invoke func = invoke iofunc
        where 
            iofunc :: String -> IO String
            iofunc = return . func

instance CommandLike ([String] -> IO [String]) where
    invoke func _ input = return $ CommandResult linedfunc (return $ Exited ExitSuccess)
        where 
            linedfunc = func (lines input) >>= return . unlines

instance CommandLike ([String] -> [String]) where
    invoke func = invoke (unlines . func . lines)

class RunResult a where
    run :: CommandLike b => b -> a 

setUpCommand :: CommandLike a => a -> IO CommandResult
setUpCommand cmd = do 
    closefds <- newMVar []
    invoke cmd closefds []

instance RunResult (IO ()) where
    run cmd =  run cmd >>= checkResult

instance RunResult (IO ProcessStatus) where
    run cmd = do 
        res <- setUpCommand cmd 
        output <- cmdOutput res 
        putStr output
        getExitStatus res

instance RunResult (IO Int) where
    run cmd =  do 
        rc <- run cmd
        case rc of 
            Exited ExitSuccess -> return 0 
            Exited (ExitFailure x) -> return x
            Terminated x _ -> return $ 128 + fromIntegral x
            Stopped x -> return $ 128 + fromIntegral x

instance RunResult (IO Bool) where
    run cmd =  do 
        rc <- run cmd 
        return $ (rc :: Int) == 0

instance RunResult (IO [String]) where
    run cmd = do 
        r <- run cmd 
        return $ lines r 

instance RunResult (IO String) where
    run cmd = do 
        res <- setUpCommand cmd 
        output <- cmdOutput res 
        evaluate $ length output
        ec <- getExitStatus res 
        checkResult ec
        return output

checkResult :: ProcessStatus -> IO ()
checkResult (Exited ExitSuccess) = return ()
checkResult x = fail $ show x

runIO' :: CommandLike a => a -> IO ()
runIO' = run

-- utility functions --
cd = setCurrentDirectory

echo :: String -> String -> String
echo = const 

grep :: String -> [String] -> [String]
grep pat = filter (`ismatch` pat)
    where ismatch = (=~) :: String -> String -> Bool

mkdir :: FilePath -> IO ()
mkdir = createDirectory

uniq :: String -> String
uniq = unlines . nub . lines

wcL, wcW :: [String] -> [String]
wcL inp = [show (genericLength inp::Integer)]

wcW inp = [show ((genericLength . words . unlines $ inp)::Integer)]

sortLines :: [String] -> [String]
sortLines = sort

countLines :: String -> IO String
countLines = return . (++) "\n" . show . length . lines
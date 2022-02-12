-- this is also the updated version of RunProcessSimple of the book
-- credit to Adam Yin who provided this version in the comment section
-- NOTE: this code still has error for piping
--       ex: 
--          > runIO $ ("ls", ["/usr"]) -|- ("grep", ["^l"])
--           <interactive>: <file descriptor: 12>: commitBuffer: illegal operation (handle is closed)
--           *** Exception: user error (Exited: Exited (ExitFailure 1))


{-# LANGUAGE FlexibleInstances#-}

module RunProcessSimple where

import System.Process hiding (createPipe)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException)
import System.IO 
import System.Exit
import Text.Regex.Base 
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types

-- the type for running external commands. The first part of the tuple is
-- the program name. The list represents the command-line parameters to pass
-- to the command.
type SysCommand = (String, [String])

-- the result of running any command
data CommandResult = CommandResult {
    cmdOutput :: IO String,
    getExitStatus :: IO ProcessStatus
}

-- type for handling global list of FD to always close in the clients
type CloseFDs = MVar [Fd]

class CommandLike a where
    -- Given the command and a String representing input, invokes the command.
    -- Returns a String representing the output of the command.
    invoke :: a -> CloseFDs -> String -> IO CommandResult

instance CommandLike SysCommand where
    invoke (cmd, args) closefds input = do 
        -- Create two pipes: one to handle 'stdin' and the other for 'stdout'.
        -- We do not redirect 'stderr' in this program.
        (stdinread, stdinwrite) <- createPipe
        (stdoutread, stdoutwrite) <- createPipe

        -- Add the parent FDs to this list because we always need to close them in the clients
        addCloseFDs closefds [stdinwrite, stdoutread]

        -- Grab the closed FDs list and fork the child
        childPID <- withMVar closefds (\fds ->
            forkProcess $ child fds stdinread stdoutwrite)

        -- On the parent, close the client-side FDs
        closeFd stdinread
        closeFd stdoutwrite

        -- write the input to the command
        stdinhdl <- fdToHandle stdinwrite
        forkIO $ do hPutStr stdinhdl input 
        hClose stdinhdl

        -- prepare receiving output from the command
        stdouthdl <- fdToHandle stdoutread

        -- set up the function to call when ready to wait for the child to exit
        let waitfunc = do 
            status <- getProcessStatus True False childPID
            case status of 
                Nothing -> fail $ "Error: Nothing from getProcessStatus"
                Just ps -> do 
                    removeCloseFDs closefds [stdinwrite, stdoutread]
                    return ps 

        return $ CommandResult {
            cmdOutput = hGetContents stdouthdl,
            getExitStatus = waitfunc
        }
      where
        child closefds stdinread stdoutwrite = do 
            -- Copy our pipes over the regular stdin/stdout FDs
            dupTo stdinread stdInput
            dupTo stdoutwrite stdOutput

            -- close the original pipe FD
            closeFd stdinread
            closeFd stdoutwrite

            -- Close all the open FDs we inherited from the parent
            mapM_ (\fd -> catch (closeFd fd)
                ((const $ return ()) :: SomeException -> IO ()))
                closefds

            -- start the program
            executeFile cmd True args Nothing

-- Add FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds = 
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)


-- remove FDs from the list
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem = 
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)
    where
        procfdlist fdlist [] = fdlist
        procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs 

        removefd [] _ = []
        removefd (x:xs) fd 
            | fd == x = xs 
            | otherwise = x : removefd xs fd

-- Type representing a pipe. A 'PipeCommand' consists of a source and a
-- destination part, both of which must be instance of 'CommandLike'.                            
data PipeCommand src dst = PipeCommand src dst

-- A convenient function for creating a 'PipeCommand'
(-|-) :: (CommandLike a, CommandLike b) => a -> b -> PipeCommand a b 
(-|-) = PipeCommand

-- Make 'PipeCommand' runnable as a command
instance (CommandLike a, CommandLike b) => CommandLike (PipeCommand a b) where
    invoke (PipeCommand src dest) closefds input = do 
        res1 <- invoke src closefds input
        output1 <- cmdOutput res1
        res2 <- invoke dest closefds output1
        return $ CommandResult (cmdOutput res2) (getEC res1 res2)

-- Given two 'CommandResult' items, evaluate the exit codes for both and
-- then return a "combined" exit code. This will be 'ExitSuccess' if both
-- exited successfully. Otherwise, it will reflect the first error encountered.        
getEC :: CommandResult -> CommandResult -> IO ProcessStatus
getEC src dest = do 
    sec <- getExitStatus src
    dec <- getExitStatus dest
    case sec of 
        Exited ExitSuccess -> return dec 
        x -> return x 

-- execute a CommandLike
runIO :: CommandLike a => a -> IO ()
runIO cmd = do 
    -- initialize closefds list
    closefds <- newMVar []

    -- invoke the command
    res <- invoke cmd closefds []

    -- process its output
    output <- cmdOutput res 
    putStr output

    -- wait for termination and exit status
    ec <- getExitStatus res 
    case ec of 
        Exited ExitSuccess -> return ()
        x -> fail $ "Exited: " ++ show x


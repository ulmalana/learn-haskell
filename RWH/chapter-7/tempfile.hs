import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
-- import System.IO.Error (catch)
import Control.Exception (finally, catch, IOException)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = do

    -- greeting 
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temp file at " ++ tempname

    -- check the inital position
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos

    -- write to temp file
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++
                show (length tempdata) ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata

    -- get new position
    pos <- hTell temph -- not changing pos, but rather point to different val(?)
    putStrLn $ "After writing, my new position is " ++ show pos

    -- seek the begining of file and display it
    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0 

    -- lazy read to entire file
    c <- hGetContents temph

    -- copy file byte-for-byte to stdout + \n
    putStrLn c 

    -- display as haskell literal
    putStrLn $ "Which could be expressed as this Haskell literal: "
    print c


-- new function to handle the getTemporaryDir
-- used in line 57
getTempdir :: IO String 
getTempdir =   catch getTemporaryDirectory handler
  where
    handler :: IOException -> IO String
    handler = \ _ -> return "." 


withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a 
withTempFile pattern func = do 
                        -- catch takes two args: one to run, and another if the first raised exception
                        tempdir <- getTempdir   
                        (tempfile, temph) <- openTempFile tempdir pattern

                        -- finally takes two actions:
                        -- one to run, and another action to run after the first (regardless of any exception by the first)
                        -- this ensures to delete temp file after some action
                        finally (func tempfile temph)   -- first action
                                (do hClose temph        
                                    removeFile tempfile)

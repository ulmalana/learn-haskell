-- updated version
-- credit to Adam Yin

import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L 
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)

import Codec.Compression.GZip (compress)

main = do 
    maybeLine <- runInputT defaultSettings $ getInputLine "Enter a file to compress: "
    case maybeLine of 
        Nothing -> return ()
        Just "" -> return ()
        Just name -> do 
            handle (print :: SomeException -> IO ()) $ do 
                content <- L.readFile name 
                forkIO (compressFile name content)
                return ()
            main 
  where
    compressFile path = L.writeFile (path ++ ".gz") . compress
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    (
        HandleIO,
        Handle,
        IOMode(..),
        runHandleIO,
        openFile,
        hClose,
        hPutStrLn
    ) where

import System.IO (Handle, IOMode(..))
import qualified System.IO
import Control.Monad
import Control.Monad.Trans (MonadIO(..))
import System.Directory (removeFile) 

newtype HandleIO a = HandleIO { runHandleIO :: IO a } deriving (Monad)

instance Functor HandleIO where
    fmap = liftM

instance Applicative HandleIO where
    pure = return
    (<*>) = ap 

instance MonadIO HandleIO where
    liftIO = HandleIO

openFile :: FilePath -> IOMode -> HandleIO Handle 
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "halohalo"
    hClose h

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do 
    safeHello path 
    liftIO (removeFile path)
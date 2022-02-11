{-# LANGUAGE DeriveDataTypeable #-}

import Data.Dynamic
import qualified Control.Exception as E

data SqlError = SqlError {
        seState :: String,
        seNativeError :: Int,
        seErrorMsg :: String
    } deriving (Eq, Show, Read, Typeable)

instance E.Exception SqlError

catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = E.catch

handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip E.catch

handleSqlError :: IO a -> IO a 
handleSqlError action =
    catchSql action handler 
    where handler e = fail ("SQL error: " ++ show e)


throwSqlError :: String -> Int -> String -> a 
throwSqlError state nativeError errorMsg =
    E.throw (SqlError state nativeError errorMsg)

throwSqlErrorIO :: String -> Int -> String -> IO a 
throwSqlErrorIO state nativeError errorMsg =
    E.evaluate (throwSqlError state nativeError errorMsg)
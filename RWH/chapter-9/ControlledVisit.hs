import Control.Monad (forM, liftM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents)
import Data.Time.Clock (UTCTime(..))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.FilePath ((</>))

data Info = Info {
          infoPath :: FilePath
        , infoPerms :: Maybe Permissions
        , infoSize :: Maybe Integer
        , infoModTime :: Maybe UTCTime 
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info 
getInfo path = do 
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

traverseC :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseC order path = do 
    names <- getUsefulContents path 
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do 
        if isDirectory info && infoPath info /= path 
            then traverseC order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do 
    names <- getDirectoryContents path 
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)


-- less dense version of traverse (usually done by less experienced haskell programmer)
traverseVerbose order path = do 
        names <- getDirectoryContents path
        let usefulNames = filter (`notElem` [".", ".."]) names
        contents <- mapM getEntryName ("" : usefulNames)
        recursiveContents <- mapM recurse (order contents)
        return (concat recursiveContents)
        where getEntryName name = getInfo (path </> name)
              isDirectory info = case infoPerms info of 
                                    Nothing -> False
                                    Just perms -> searchable perms
              recurse info = do 
                    if isDirectory info && infoPath info /= path 
                        then traverseVerbose order (infoPath info)
                        else return [info]
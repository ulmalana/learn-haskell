import Data.Time.Clock (UTCTime(..))
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import System.FilePath.Posix (takeFileName)
import Data.Char (toLower)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Monad (liftM)

data Info = Info {
          infoPath :: FilePath
        , infoPerms :: Maybe Permissions
        , infoSize :: Maybe Integer
        , infoModTime :: Maybe UTCTime 
    } deriving (Eq, Ord, Show)

data Iterate seed = Done        { unwrap :: seed }
                  | Skip        { unwrap :: seed }
                  | Continue    { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

-- to call this fn : foldTree atMostThreePictures []

foldTree :: Iterator a -> a -> FilePath -> IO a 
foldTree iter initSeed path = do 
    endSeed <- fold initSeed path 
    return (unwrap endSeed)
    where 
        fold seed subpath = getUsefulContents subpath >>= walk seed 
        walk seed (name:names) = do 
            let path' = path </> name 
            info <- getInfo path'
            case iter seed info of 
                done@(Done _) -> return done
                Skip seed' -> walk seed' names
                Continue seed'
                    | isDirectory info -> do 
                        next <- fold seed' path'
                        case next of 
                            done@(Done _) -> return done
                            seed'' -> walk (unwrap seed'') names 
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info 
    | length paths == 3
        = Done paths
    | isDirectory info && takeFileName path == ".svn"
        = Skip paths
    | extension `elem` [".jpg", ".png"] 
        = Continue (path:paths)
    | otherwise
        = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info


countDirectories count info = 
    Continue (if isDirectory info
                then count + 1
                else count)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getInfo :: FilePath -> IO Info
getInfo path = do 
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do 
    names <- getDirectoryContents path 
    return (filter (`notElem` [".", ".."]) names)
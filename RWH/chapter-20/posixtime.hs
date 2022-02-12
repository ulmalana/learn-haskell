-- updated version of posixtime.hs of the book
-- since System.Time is deprecated

import System.Posix.Files
import System.Posix.Types
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

-- given a file, returns (access time, modification time, and last change time)
getTimes :: FilePath -> IO (LocalTime, LocalTime, LocalTime)
getTimes fp = do 
    stat <- getFileStatus fp 
    let atime = toLocal $ accessTimeHiRes stat 
        mtime = toLocal $ modificationTimeHiRes stat 
        ctime = toLocal $ statusChangeTimeHiRes stat 
    toTriple <$> atime <*> mtime <*> ctime 
  where
    toLocal = toLocalTime . toUTC
    toTriple x y z = (x,y,z)

toUTC :: POSIXTime -> UTCTime
toUTC = posixSecondsToUTCTime

toLocalTime :: UTCTime -> IO LocalTime
toLocalTime t = do 
    tz <- getTimeZone t 
    return $ utcToLocalTime tz t
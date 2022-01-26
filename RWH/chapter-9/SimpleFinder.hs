import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
-- p below is predicate
simpleFind p path = do 
    names <- getRecursiveContents path 
    return (filter p names)
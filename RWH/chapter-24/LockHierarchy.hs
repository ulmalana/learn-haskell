import Control.Concurrent

nestedModification outer inner = do 
    modifyMVar_ outer $ \x -> do 
        yield 
        modifyMVar_ inner $ \y -> return (y + 1)
        return (x+1)
    putStrLn "done"

main = do 
    a <- newMVar 1
    b <- newMVar 2
    forkIO $ nestedModification a b 
    forkIO $ nestedModification b a
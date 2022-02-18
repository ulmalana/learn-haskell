import Control.Concurrent
import Control.Concurrent.Chan

chanExample = do 
    ch <- newChan
    forkIO $ do
        writeChan ch "hallo hehe"
        writeChan ch "now i quit"
    readChan ch >>= print
    readChan ch >>= print
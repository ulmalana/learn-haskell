import Data.Bits 
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String -> HandlerFunc -> IO ()
serveLog port handlerfunc = withSocketsDo $ do 
    addrinfos <- getAddrInfo 
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    procMessages sock 
        where procMessages sock = do 
                                    (msg, _, addr) <- recvFrom sock 1024
                                    handlerfunc addr msg
                                    procMessages sock 

plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg

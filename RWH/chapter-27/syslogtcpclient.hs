import Data.Bits 
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes
import System.IO 

data SyslogHandle = SyslogHandle {
    slHandle :: Handle,
    slProgram :: String
}

openlog :: HostName -> String -> String -> IO SyslogHandle
openlog hostname port progname = do 
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1

    connect sock (addrAddress serveraddr)

    h <- socketToHandle sock WriteMode

    hSetBuffering h (BlockBuffering Nothing)
    return $ SyslogHandle h progname

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg = do 
    hPutStrLn (slHandle syslogh) sendmsg
    hFlush (slHandle syslogh)
  where code = makeCode fac pri 
        sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ": " ++ msg

closelog :: SyslogHandle -> IO ()
closelog syslogh = hClose (slHandle syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac 
        pricode = fromEnum pri 
    in (faccode `shiftL` 3) .|. pricode
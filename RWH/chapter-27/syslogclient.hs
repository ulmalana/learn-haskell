import Data.Bits 
import Network.Socket
--import qualified Network.Socket.ByteString as B
import Network.BSD
import Data.List
import SyslogTypes

data SyslogHandle = SyslogHandle {
    slSocket :: Socket,
    slProgram :: String,
    slAddress :: SockAddr
}

openlog :: HostName -> String -> String -> IO SyslogHandle
openlog hostname port progname = do 
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    return $ SyslogHandle sock progname (addrAddress serveraddr)

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    sendstr sendmsg 
    where code = makeCode fac pri 
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ": " ++ msg 
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do 
            sent <- sendTo (slSocket syslogh) omsg
                    (slAddress syslogh)
            sendstr (genericDrop sent omsg)

closelog :: SyslogHandle -> IO ()
closelog syslogh = close (slSocket syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac 
        pricode = fromEnum pri 
    in (faccode `shiftL` 3) .|. pricode
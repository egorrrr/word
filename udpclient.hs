-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
module UDPClient (syslog, openlog, closelog,test)  where

import Data.Bits (shiftL, (.|.))
import Network.Socket ( SocketType (..), SockAddr (..), Socket (..) 
                       , sClose, sendTo, addrAddress 
                       , addrFamily, withSocketsDo, getAddrInfo
                       , socket)                             
import Network.BSD (HostName (..), defaultProtocol)
import Data.List (genericDrop)
import SyslogTypes
--import Data.Encoding (encodeString) -- failure on build GHC-7
--import Data.Encoding.UTF8
import Codec.Binary.UTF8.String (encodeString)

data SysLogHandle =
  SysLogHandle {  slSocket :: Socket
                , slProgram :: String  
                , slAddress :: SockAddr}  

openlog :: HostName 
        -> String            -- port number
        -> String            -- name to log under
        -> IO SysLogHandle   
        
openlog hostname port  progname =  withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr ) Datagram defaultProtocol  
  return $ SysLogHandle sock progname  (addrAddress serveraddr)

syslog :: SysLogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
  sendstr sendmsg
  where 
    code = makeCode fac pri
    sendmsg = -- "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ":" ++ 
              -- head $ fmap (encodeString UTF8) [msg] -- :Encoding: 
              encodeString msg
              
    sendstr :: String -> IO ()
    sendstr [] = return ()
    sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg 
                                     (slAddress syslogh)
                      sendstr (genericDrop sent omsg) 
  
closelog :: SysLogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
  let faccode = codeOfFac fac
      pricode = fromEnum pri
      in
        (faccode `shiftL` 3) .|. pricode

test  = do
  h <- openlog "localhost" "3000" "hskserv"
  syslog h USER INFO  "haskell: knock, knock"
  closelog h

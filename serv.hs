-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes
import UDPClient (openlog, syslog, closelog)
import System.Process (runCommand, waitForProcess, getProcessExitCode)
import System.Exit (exitWith)

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String 
          -> HandlerFunc
          -> IO ()

serveLog port handlerfunc = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just ( defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)        
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  procMessages sock
  where
    procMessages sock = do
      (msg, _, addr) <- recvFrom sock 1024
      handlerfunc addr msg
      procMessages sock

plainHandler :: HandlerFunc
plainHandler addr msg = do 
  case sender == "print.exe" of
    True  -> do 
             (host,port,id,act,body)  <- parse 
             case act of
               "shellExecute" -> do  pid <- runCommand body
                                     res <- waitForProcess pid -- >>= exitWith) 
                                     sendMsg host port $ "resp to "  ++ id ++ ": " ++ show res
               _              -> do sendMsg host port $ 
                                      "Undefined command: " ++ act ++  ", msgId: " ++ id
               --  sendMsg $ id ++ ":" ++ "[entry]Поиск организации[act]showGrid" 
  
    False -> putStrLn $  "ignored: " ++ msg
             -- append log
             

    where tail' ls     = if length ls == 0 then [] else tail ls 
          sender = takeWhile (/=':') msg  
          trim s  = concat $ words $ unwords [s]
          parse  = do msg'  <- return $ dropWhile (/=':') msg
                      (host,host') <- return $ span (/=':') $ tail' msg'
                      (port,port') <- return $ span (/=':') $ tail' host'
                      (id  ,id')   <- return $ span (/=':') $ tail' port'
                      (act ,act')  <- return $ span (/=']') $ tail' $ dropWhile (/='[') id'
                      body         <- return $ tail' act'
                      return (trim host,trim port,trim id,trim act,body)
            
sendMsg host port msg = do
  h <- openlog host port "serv.exe"
  syslog h USER INFO msg
  closelog h

run = serveLog "3000" plainHandler

main = run

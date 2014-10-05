module Network.UDP.HolePunch where

{-
P2P hole punching strategy as implemented by
chownat http://samy.pl/chownat/
-}


{-

get a udp connenction flowing
-}

import Network.Socket
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Exit
import System.Environment
import Data.IP
import Control.Concurrent
import Prelude as P
import Control.Monad
import Network.BitTorrent.DHT.Utils


{- quick chownat attempt
send through a nat and receive through a nat...

-}

chownatDefPort = 2203

chownat = do
  args <- getArgs
  let remoteIp = (read (args !! 0)  :: IPv4)
  forkIO $ slowPunch remoteIp
  -- listen for conns
  sock <- socket AF_INET Datagram 0
  bindSocket sock (SockAddrInet chownatDefPort iNADDR_ANY)
  putStrLn "oh yeah..."
  forever $ do
    (mesg, recv_count, client) <- recvFrom sock maxline
    putStrLn $ "got message " ++ (show mesg)
  return ()


slowPunch ipv4Addr =do
  sock <- socket AF_INET Datagram 0
  connect sock (SockAddrInet chownatDefPort (toWord32 $ P.map fromIntegral $ fromIPv4 ipv4Addr))
  forever $ do
    send sock "hello"
    threadDelay (10 ^ 7)


clientUDP :: IO ()
clientUDP = do
  withSocketsDo $ do
    putStrLn "running"
    sock <- socket AF_INET Datagram 0
    connect sock (SockAddrInet echoPort (toWord32 [127, 0, 0, 1]))
    send sock "hello"
    putStrLn "sent message "
    resp <- recv sock 2
    putStrLn resp




echoPort = 9901
maxline = 1500

--
-- The daemon infrastructure
--

runMain :: IO ()
runMain = do
           putStrLn "running main"
           pid <- forkProcess child
           exitImmediately ExitSuccess


child :: IO ()
child = do
           -- Set up the working directory, mask and standard i/o
           -- for a daemon process (these will be inherited by
           -- the forked process):

           changeWorkingDirectory "/"
           setFileCreationMask 0

           mapM_ closeFd [stdInput, stdOutput, stdError]
           nullFd <- openFd "/dev/null" ReadWrite Nothing  defaultFileFlags
           mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]

           closeFd nullFd

           createSession     -- This child becomes a process and session
                             -- group leader. This prevents the child of
                             -- this process (the daemon) from
                             -- ever getting a controlling terminal.
           pid' <- forkProcess echoserver

           exitImmediately ExitSuccess

--
-- The echo server daemon
--

echoserver :: IO ()
echoserver = do
           withSocketsDo $ do
                   sock <- socket AF_INET Datagram 0
                   bindSocket sock (SockAddrInet echoPort iNADDR_ANY)
                   socketEcho sock


socketEcho :: Socket -> IO ()
socketEcho sock = do
           (mesg, recv_count, client) <- recvFrom sock maxline
           putStrLn $ "got message " ++ (show mesg)
           send_count <- sendTo sock mesg client
           socketEcho sock


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.BitTorrent.DHT (
  ) where

import Prelude as P
import Network.Socket
import Data.BEncode
import Data.Word
import Data.LargeWord (Word160)
import Data.ByteString.Lazy as BSL
import Data.Binary
import Network.KRPC.Manager as KRPC
import Network.KRPC.Method
import Data.Default
import Data.Word
import Data.Bits
import Data.Text as DT
import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Concurrent
import Network.BitTorrent.DHT.Message
import Network.BitTorrent.Core.NodeInfo
import Data.Maybe
import Data.Torrent.InfoHash
import Data.IP
import Data.Either.Unwrap


newtype Pang = Pang Text deriving (BEncode, Typeable, Show)
newtype Pong = Pong Text deriving (BEncode, Typeable, Show)

instance KRPC Pang Pong where
     method = "ping"



handlers :: [Handler IO]
handlers =
 [ handler $ \ _ (Pang m)     -> return $ Pong "hello back"
 ]

handleMsg ::  SockAddr -> Pang -> IO Pong
handleMsg addr m = return $ Pong "hello"

servAddress = SockAddrInet 7002 (toWord32 [127, 0, 0, 1])

runServerManager = do
  withManager def servAddress handlers $ runReaderT $ do
    KRPC.listen
    liftIO $ threadDelay 10000000
  return ()



instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()


    -- KRPC.getQueryCount


clientAddress = SockAddrInet 7001 (toWord32 [127, 0, 0, 1])

runClientManager = do
  withManager def clientAddress handlers  $ runReaderT $ do
    KRPC.listen 
    r <- KRPC.query servAddress (Pang "hello")
    liftIO $ P.putStrLn (show r)
    return ()
  return ()  


localID = "dddddddddddddddddddd"
btHandlers :: [Handler IO]
routerAddress = SockAddrInet 6881 $ toWord32 $ [67, 215, 246, 10]

btHandlers = [handler $ \ _ (Query _ Ping) -> return $ Response localID Ping]


peerOfTheInternets = SockAddrInet 42428 $ toWord32 [222, 166, 67, 27]

otherPeer = SockAddrInet 21037 $ toWord32 [211, 24, 52, 169]

localAddr = SockAddrInet 6881 $ toWord32 $ [192, 168, 1, 68]

pornHash = fromJust $ textToInfoHash $ DT.pack "06d88c5a717d84ef15dece52375491aeb23fd5ab"

contactBTNode :: IO (Response (GotPeers IPv4))
contactBTNode = do
  withManager def localAddr  btHandlers $ runReaderT $ do
    KRPC.listen 
    KRPC.query peerOfTheInternets $ Query localID $ GetPeers pornHash

getStuff = do
  (Response _ r) <- contactBTNode
  let ps = fromLeft $ peers r
  P.putStrLn $ show $ P.zip [1..] $ P.map nodeId $ P.take 9 $ ps
  --P.putStrLn $ show $ fromLeft $ peers r
{-
type InfoHash = Word160
type NodeID = Word160
type Token = ByteString

data Message = Message {transactionID :: ByteString, content :: Query}  | ErrorM ErrorClass ByteString
data Query = Ping | GetPeers InfoHash | FindNode NodeID | AnnouncePeer InfoHash Word16 Token
           | Pong | PeersReply | FindNodeReply | AnnounceReply
data ErrorClass = GenericError | ServerError | ProtocolError | MethodUnknown

mydht :: IO ()
mydht = P.putStrLn "Wtf"
-}

toWord32 :: [Word8] -> Word32
toWord32 = P.foldr (\o a -> (a `shiftL` 8) .|. fromIntegral o) 0

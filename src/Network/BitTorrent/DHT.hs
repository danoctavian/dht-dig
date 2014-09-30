
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
import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
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
import System.Timeout
import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.IP
import Control.Exception hiding (Handler)
import Control.Monad.Loops
import Data.Serialize as DS
import Network.BitTorrent.Core.PeerAddr
import Control.Concurrent.Async

import Network.BitTorrent.DHT.RoutingTable as RT
import Network.BitTorrent.DHT.RoutingTable as Utils


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

localAddr = SockAddrInet 6881 $ toWord32 $ [192, 168, 1, 68] -- [192, 168, 0, 130] -- 

randPeer = SockAddrInet 9549 $ toWord32 [106, 136, 23, 18] 

pornHash = fromJust $ textToInfoHash $ DT.pack "06d88c5a717d84ef15dece52375491aeb23fd5ab"

-- random node from the internets
bootstrapNodes = [startNode00]

startNode00 = NodeInfo "\EM?4\161\241\187\233\235\179\166\219<\135\f>\153$^\r\241" $ NodeAddr  (toIPv4 [222, 166, 67, 27]) 42428
startNode01 = NodeInfo "\NUL=Y49\207\138\143\211B\180\206!t\tQV\223\235j" $ NodeAddr (toIPv4 [123, 202, 126, 83]) 36740

startNode02 = NodeInfo "\ACK\216\142y\247\187\150\217\176\ETX\180\a\186\174L\SUB\ENQ\247\142&" $ NodeAddr (toIPv4 [46, 42, 61, 252]) 10097

getPeers :: SockAddr -> InfoHash -> IO (Response (GotPeers IPv4))
getPeers addr infoHash = do
  withManager def localAddr  btHandlers $ runReaderT $ do
    KRPC.listen 
    KRPC.query addr $ Query localID $ GetPeers infoHash

pingBTNode :: SockAddr -> IO (Response Ping)
pingBTNode addr = do
  withManager def localAddr  btHandlers $ runReaderT $ do
    KRPC.listen 
    KRPC.query addr $ Query localID $ Ping

getStuff = do
  (Response _ r) <- getPeers peerOfTheInternets pornHash
  let ps = fromLeft $ peers r
  P.putStrLn $ show $ P.zip [1..] $ P.map nodeId $ P.take 9 $ ps

pingANode = do
  (Response nid r) <- pingBTNode peerOfTheInternets
  P.putStrLn $ show $ nid

closePeers = 8

nodeAddrToSockAddr (NodeAddr host port) = SockAddrInet port (toHostAddress host)

addNodes = P.foldl (\rt n -> RT.insert ((\(NodeId i) -> i) . nodeId $ n) n rt)


bsToInfoHash = fromJust . textToInfoHash . DT.pack . DBC.unpack

-- source of peers
-- the logic of getting peers
peerSource bootstrapNodes infoHashHex = do
  let infoHash = bsToInfoHash infoHashHex
  let rt = addNodes RT.empty bootstrapNodes
  (P.flip iterateM_) rt $ \rt -> do
    let close = RT.closest (DS.encode infoHash) closePeers rt
    liftIO $ P.putStrLn "new iteration.. "
    maybeResp <- forM close $ \nodeInfo -> do

    --maybeResp <- liftIO $ (P.flip mapConcurrently) close $ \nodeInfo -> do
      liftIO $ P.putStrLn $ "querying node " P.++ (show nodeInfo)
      resp <- liftIO $ timeout (10 ^ 6 * 2) (getPeers (nodeAddrToSockAddr . nodeAddr $ nodeInfo) infoHash)
      when (isNothing resp) (liftIO $ P.putStrLn "timed out")
      return resp
    let responses = P.map (\(Just (Response _ resp)) -> resp) $ P.filter (not . isNothing) maybeResp
    -- insert all nodes that are not from this 
    let getNodes (unwrap, cond) = P.map unwrap $ P.filter cond $ P.map peers responses
    -- liftIO $ P.putStrLn $ show $ P.zip [1..] $ P.map nodeId $ P.concat $ getNodes (fromLeft, isLeft)
    let newRT = P.foldl addNodes rt $ getNodes (fromLeft, isLeft)
    liftIO $ P.putStrLn $ "size of node tree " P.++ (show $ RT.size newRT)
    forM (P.concat $ getNodes (fromRight, isRight))$ \i -> do
      liftIO $ P.putStrLn "FUCKING FINALLY"
      --liftIO $ threadDelay 10000000
      DC.yield i
    return newRT  


-- sink for contactable peers; ends when it finds the right peer
peerContact peerKey =  undefined

-- main program flow
-- consume peers until yu find the right one


data PeerConnException = ConnTimeout deriving (Show, Typeable)
instance Exception PeerConnException

connectTo :: MonadIO m => DBC.ByteString
     -> t -> m [PeerAddr IPv4]
connectTo infoHash peerKey = do
  result <- liftIO $ timeout (10 ^ 8) (peerSource bootstrapNodes infoHash
                   $= DCL.mapM (\v -> (liftIO $ P.putStrLn $ "THIS IS A RESULT " P.++ (show v) ) >> (return v) )
                   $$ DCL.take 10) 
  case result of 
    Just conn -> return conn
    Nothing -> liftIO $ throw ConnTimeout

attemptConnection = do
  peers <- connectTo "06d88c5a717d84ef15dece52375491aeb23fd5ab" "mygod"
  P.putStrLn $ show peers


toWord32 :: [Word8] -> Word32
toWord32 = P.foldr (\o a -> (a `shiftL` 8) .|. fromIntegral o) 0

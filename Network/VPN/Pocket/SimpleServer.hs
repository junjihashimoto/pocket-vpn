module Network.VPN.Pocket.SimpleServer where

import Network.BSD
import Network.Socket hiding (recvFrom,sendTo)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Maybe

import Network.VPN.Pocket.TUN
import Data.Serialize
import Data.Serialize.Get
import Data.Word


data Packet = PacketIPv4 {
  pSrcAddr :: Word32
, pDstAddr :: Word32
, pDat :: B.ByteString
} deriving (Show,Read)

readPacket :: B.ByteString -> Either String Packet
readPacket dat = flip runGet dat $ do
  _ <- getWord32be
  _ <- getWord32be
  _ <- getWord32be
  src <- getWord32be
  dst <- getWord32be
  return $ PacketIPv4 src dst dat

runServer :: DeviceName -> (String,String) -> (String,String) -> IO ()
runServer name (ip,port) (ip',port') = do
  ipw <- inet_addr ip
  etun <- tunOpen name
  _ <- tunUp name
--  _ <- tunSetIp name ipw
  case etun of
    Right tun -> do
      withSocketsDo $ do
        addrinfo <- getAddr Nothing port
        addrinfo' <- getAddr (Just ip') port'
        sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
        sock' <- socket (addrFamily addrinfo') Datagram defaultProtocol
        -- tunChan <- newChan
        -- sockChan <- newChan
        -- sockChan' <- newChan
        bindSocket sock (addrAddress addrinfo)
        forever $ do
          (bs,addr) <- recvFrom sock (64*1024)
          forkIO $ do
            print $ "put tun"
            print $ B.length bs
            B.hPut tun bs
          return ()
        forkIO $ forever $ do
          bs <- B.hGet tun (64*1024)
          print $ "get tun"
          print $ B.length bs
          void $ sendTo sock' bs (addrAddress addrinfo')
        return ()
    Left err -> print err
  where
    getAddr ip port = do
      addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   ip (Just port)
      maybe (fail "no addr info") return (listToMaybe addrinfos)


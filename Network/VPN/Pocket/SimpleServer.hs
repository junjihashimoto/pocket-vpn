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
import System.IO

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

runServer' :: (DeviceName -> IO (Either TError Handle)) -> DeviceName -> (String,String) -> (String,String) -> IO ()
runServer' open name (ip,port) (ip',port') = do
  ipw <- inet_addr ip
  etun <- open name
  _ <- ifUp name
  case etun of
    Right tun -> do
      hSetBuffering tun NoBuffering
      withSocketsDo $ do
        addrinfo <- getAddr Nothing port
        addrinfo' <- getAddr (Just ip') port'
        sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
        sock' <- socket (addrFamily addrinfo') Datagram defaultProtocol
        bindSocket sock (addrAddress addrinfo)
        forkIO $ forever $ do
          bs <- B.hGetSome tun (64*1024)
          void $ sendTo sock' bs (addrAddress addrinfo')
        forever $ do
          (bs,addr) <- recvFrom sock (64*1024)
          forkIO $ do
            B.hPut tun bs
    Left err -> print err
  where
    getAddr ip port = do
      addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   ip (Just port)
      maybe (fail "no addr info") return (listToMaybe addrinfos)


runTunServer :: DeviceName -> (String,String) -> (String,String) -> IO ()
runTunServer = runServer' tunOpen

runTapServer :: DeviceName -> (String,String) -> (String,String) -> IO ()
runTapServer = runServer' tapOpen

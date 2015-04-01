{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

--import Prelude hiding (FilePath)
import Options.Applicative
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Control.Monad
import System.Exit
import Network.VPN.Pocket.SimpleServer

default (T.Text)

data Command
  = SimpleTunServer {
       netDevice :: String
     , serverIp :: String
     , serverPort :: String
     , clientIp :: String
     , clientPort :: String
     , daemonize :: Bool
    }
  | SimpleTapServer {
       netDevice :: String
     , serverIp :: String
     , serverPort :: String
     , clientIp :: String
     , clientPort :: String
     , daemonize :: Bool
    }
  deriving Show

simpleTunServer :: Parser Command
simpleTunServer = SimpleTunServer
      <$> (argument str (metavar "NETDEVICE"))
      <*> (argument str (metavar "SERVERIP"))
      <*> (argument str (metavar "SERVERPORT"))
      <*> (argument str (metavar "CLIENTIP"))
      <*> (argument str (metavar "CLIENTPORT"))
      <*> flag False True (long "daemoize" <> short 'd'<> help "Enable daemonize mode")

simpleTapServer :: Parser Command
simpleTapServer = SimpleTapServer
      <$> (argument str (metavar "NETDEVICE"))
      <*> (argument str (metavar "SERVERIP"))
      <*> (argument str (metavar "SERVERPORT"))
      <*> (argument str (metavar "CLIENTIP"))
      <*> (argument str (metavar "CLIENTPORT"))
      <*> flag False True (long "daemoize" <> short 'd'<> help "Enable daemonize mode")

parse :: Parser Command
parse = subparser $ 
        command "tun-server"  (info simpleTunServer (progDesc "simple udp server")) <>
        command "tap-server"  (info simpleTapServer (progDesc "simple udp server"))
        
runCmd :: Command -> IO ()
runCmd (SimpleTunServer net host ip host' ip' _ ) = do
  runTunServer net (host,ip) (host',ip')
runCmd (SimpleTapServer net host ip host' ip' _ ) = do
  runTapServer net (host,ip) (host',ip')

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd


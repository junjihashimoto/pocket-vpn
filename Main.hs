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
  = SimpleServer String String String String String
  deriving Show

simpleServer :: Parser Command
simpleServer = SimpleServer
      <$> (argument str (metavar "NETDEVICE"))
      <*> (argument str (metavar "SERVERIP"))
      <*> (argument str (metavar "SERVERPORT"))
      <*> (argument str (metavar "CLIENTIP"))
      <*> (argument str (metavar "CLIENTPORT"))

parse :: Parser Command
parse = subparser $ 
        command "server"    (info simpleServer (progDesc "simple udp server"))

        
runCmd :: Command -> IO ()
runCmd (SimpleServer net host ip host' ip') = do
  runServer net (host,ip) (host',ip')

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd


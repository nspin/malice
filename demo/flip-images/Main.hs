{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import FlipImages

import Mal.Monad
import Mal.Protocol.TLS
import Mal.Protocol.TLS.Hybrid
import Mal.Middle.TCP
import Mal.Middle.TCP.Transparent
import Mal.Middle.Socks5

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Crypto.PubKey.RSA
import Crypto.Random
import Data.Bool
import Data.Monoid
import Data.Text (pack)
import Data.X509
import Data.X509.File
import Network.Socket hiding (recv, send)
import Network.TLS hiding (SHA256, HashSHA256)
import Options.Applicative


data Opts = Opts
    { transparent :: Bool
    , port :: PortNumber
    , certPath :: String
    , privPath :: String
    }

opts :: Parser Opts
opts = Opts
    <$> switch
        (  long "transparent"
        <> short 't'
        <> help "Whether to run as a transparent TCP proxy using netfilter instead as a SOCKS5 proxy"
        )
    <*> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 8080
        <> showDefault
        <> help "Port on which to listen"
        )
    <*> argument str (metavar "CERT_IN" <> help "Input path for root certificate")
    <*> argument str (metavar "PRIV_IN" <> help "Input path for root key")

parser = info (opts <**> helper)
    (  fullDesc
    <> progDesc "Flip images passing through HTTP and HTTPS like a real hacker."
    )

main :: IO ()
main = do
    Opts{..} <- execParser parser
    [rootSigned] <- readSignedObject certPath
    [PrivKeyRSA rootPriv] <- readKeyFile privPath
    let swapKey = makeKeySwapper (signedObject (getSigned rootSigned)) rootPriv myPriv
    chan <- newChan
    forkIO . runStderrLoggingT . filterLogger (const (> LevelDebug)) $ unChanLoggingT chan
    runChanLoggingT chan . tcpServer (SockAddrInet port 0) .
        bool socks5Handler transparentProxyHandler transparent $
            malMaybeTLS swapKey (go . fromTCPProxyCtx) (go . fromContexts)
  where
    ((_, myPriv), _) = withDRG (drgNewSeed (seedFromInteger 0)) (generate 256 3)
    go :: Vertices (LoggingT IO) -> LoggingT IO ()
    go = evalMalT' flipImages >=> either ($logError . pack) return
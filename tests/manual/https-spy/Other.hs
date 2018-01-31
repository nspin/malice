{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Spy

import Mal.Middle.Socks5
import Mal.Middle.TCP
import Mal.Middle.Vertices
import Mal.Monad
import Mal.Protocol.TLS
import Mal.Protocol.TLS.Hybrid

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Lens (view)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Trans.Unlift
import Crypto.PubKey.RSA
import Crypto.Random
import Data.Bool
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text (pack)
import Data.X509
import Data.X509.File
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.TLS hiding (SHA256, HashSHA256)
import Options.Applicative


data Opts = Opts
    { port :: PortNumber
    , certPath :: String
    , privPath :: String
    }

opts :: Parser Opts
opts = Opts
    <$> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 8080
        <> showDefault
        <> help "Port on which to listen"
        )
    <*> argument str (metavar "CERT" <> help "Input path for root certificate")
    <*> argument str (metavar "PRIV" <> help "Input path for root key")

parser = info (opts <**> helper)
    (  fullDesc
    <> progDesc "Spy on and log HTTPS session."
    )

main :: IO ()
main = do
    Opts{..} <- execParser parser
    [rootSigned] <- readSignedObject certPath
    [PrivKeyRSA rootPriv] <- readKeyFile privPath
    let swapKey = makeKeySwapper (signedObject (getSigned rootSigned)) rootPriv myPriv
    chan <- newChan
    forkIO . runStderrLoggingT . filterLogger (const (> LevelDebug)) $ unChanLoggingT chan
    portCount <- newMVar 13337
    let go :: Vertices (LoggingT IO) -> LoggingT IO ()
        go vs = do
            port <- liftIO $ takeMVar portCount
            liftIO $ putMVar portCount (port + 1)
            eps <- loopback port (passive vs)
            evalEveT' spy eps >>= either ($logError . pack) return
    runChanLoggingT chan . tcpServer (SockAddrInet port 0) . socks5Handler $
        malMaybeTLS swapKey (go . fromTCPProxyCtx) (go . fromContexts)
  where
    ((_, myPriv), _) = withDRG (drgNewSeed (seedFromInteger 0)) (generate 256 3)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Mal.Middle.TCP
    ( Connection(..)
    , tcpServer
    , TCPProxyCtx(..)
    , fromTCPProxyCtx
    ) where

import Mal.Monad hiding (recv)

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Unlift
import Data.Monoid
import Data.Text (pack)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString


data Connection = Connection
    { connSock :: Socket
    , connAddr :: SockAddr
    } deriving Show


-- | Simple forking TCP server. May need to be nested under a call to 'withSocketsDo'
tcpServer :: (MonadIO m, MonadCatch m, MonadMask m, MonadLogger m, MonadBaseUnlift IO m) => SockAddr -> (Connection -> m ()) -> m ()
tcpServer addr handler = do
    sock <- liftIO $ do
        sock <- socket AF_INET Stream 0
        setSocketOption sock ReuseAddr 1
        bind sock addr
        listen sock 0
        return sock
    $logInfo $ "listening on " <> pack (show addr)
    let cleanup = do
            $logInfo $ "closing sock on " <> pack (show addr)
            liftIO $ close sock
    flip finally cleanup . forever $ do
        (s, clientAddr) <- liftIO $ accept sock
        $logInfo $ "accepted connection from " <> pack (show clientAddr)
        let disconnected ex = $logWarn $
                "exception from " <> pack (show clientAddr) <> ": " <> pack (show (ex :: SomeException))
            cleanup = do
                $logInfo $ "closing sock to " <> pack (show clientAddr)
                liftIO $ close s
        run <- askRunBase
        liftIO . forkIO . run $ handler (Connection s clientAddr) `catch` disconnected `finally` cleanup


data TCPProxyCtx = TCPProxyCtx
    { clientSock :: Socket -- ^ Socket connected to client
    , serverSock :: Socket -- ^ Socket connected to server
    }


fromTCPProxyCtx :: MonadIO m => TCPProxyCtx -> Vertices m
fromTCPProxyCtx (TCPProxyCtx client server) = Vertices
    (Vertex (liftIO $ recv client 4096) (liftIO . sendAll client))
    (Vertex (liftIO $ recv server 4096) (liftIO . sendAll server))

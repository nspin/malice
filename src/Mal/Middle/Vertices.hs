{-# LANGUAGE FlexibleContexts #-}

module Mal.Middle.Vertices
    ( proxyBoth
    , loopback
    ) where

import Mal.Monad

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Unlift
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import qualified Data.ByteString as B

-- | Modify 'Vertices' to copy raw recved bytes through the loopback interface
-- on the given port. This is a useful hack for inspecting TLS sessions in, for
-- example, wireshark, as if it were a TCP session. This is particularly useful
-- for debugging parsers.
loopback :: (MonadIO m, MonadMask m, MonadBaseUnlift IO m) => Vertices m -> PortNumber -> m (Vertices m)
loopback (Vertices alice bob) port = do
    sock <- liftIO $ do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock addr
        listen sock 0
        return sock
    let client = do
            s <- liftIO $ socket AF_INET Stream defaultProtocol
            liftIO $ connect s addr
            return $ do
                bs <- edgeIn alice
                liftIO $ send s bs
                return bs
        server = do
            (s, _) <- liftIO $ accept sock
            return $ do
                bs <- edgeIn bob
                liftIO $ send s bs
                return bs
    run <- askRunBase
    (al, bo) <- liftIO $ concurrently (run client) (run server)
    return $ Vertices (alice{edgeIn=al}) (bob{edgeIn=bo})
  where
    addr = SockAddrInet port 0

-- | The identity action on 'Vertices', that just proxies from one side to the
-- other in both direction
proxyBoth :: (MonadIO m, MonadBaseUnlift IO m) => Vertices m -> m ()
proxyBoth (Vertices alice bob) = do
    run <- askRunBase
    liftIO $ concurrently_ (run $ go alice bob) (run $ go bob alice)
  where
    go from to = do
        bs <- edgeIn from
        edgeOut to bs
        unless (B.null bs) (go from to)

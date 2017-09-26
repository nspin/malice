{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Mal.Middle.TCP.Transparent
    ( transparentProxyHandler
    ) where

import Mal.Middle.TCP
import Mal.Middle.Socket
import Mal.Extra.Binascii

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Unlift
import Data.Monoid
import Data.Function
import Data.Text (pack)
import Network.Socket hiding (recv, send)

-- | Given a raw connection to a client, obtain a connection to the client's
-- original destination from netfilter and yield control to a man in the middle.
transparentProxyHandler :: (MonadIO m, MonadCatch m, MonadMask m, MonadLogger m, MonadBaseUnlift IO m)
    => (TCPProxyCtx -> m ()) -> Connection -> m ()
transparentProxyHandler mitm (Connection client addr) = do
    bs <- liftIO $ getSockOpt client sOL_IP sO_ORIGINAL_DST 20
    saddr <- uncurry SockAddrInet <$> liftIO (getOriginalDst client)
    server <- liftIO $ socket AF_INET Stream defaultProtocol
    liftIO $ connect server saddr
    let handle ex = $(logWarn) $
            "mitm from " <> pack (show addr) <> " threw exception: " <> pack (show (ex :: SomeException))
        cleanup = liftIO $ close server
    mitm (TCPProxyCtx client server) `catch` handle `finally` cleanup

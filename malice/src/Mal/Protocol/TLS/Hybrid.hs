{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Mal.Protocol.TLS.Hybrid
    ( malMaybeTLS
    ) where

import Mal.Middle.Socket
import Mal.Middle.TCP
import Mal.Protocol.TLS

import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Unlift
import Data.Bits
import Network.TLS
import qualified Data.ByteString as B


malMaybeTLS :: (MonadIO m, MonadLogger m, MonadBaseUnlift IO m)
            => KeySwapper
            -> (TCPProxyCtx -> m ())
            -> (Contexts -> m ())
            -> TCPProxyCtx
            -> m ()
malMaybeTLS swapKey ifTCP ifTLS ctx@TCPProxyCtx{..} = do
    peek <- liftIO $ recvWithFlags (mSG_PEEK .|. mSG_WAITALL) clientSock 3
    let isTLS =  B.length peek == 3
              && B.index peek 0 == 22
              && B.index peek 1 == 3
              && elem (B.index peek 2) [1, 2, 3]
    if isTLS
     then malTLS swapKey (getBackend clientSock) (getBackend serverSock) ifTLS
     else ifTCP ctx

{-# LANGUAGE RecordWildCards #-}

module Mal.Protocol.TLS.Backend
    ( makeHybrid
    ) where

import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Network.TLS

-- | Transform a 'Backend' with some initial data to be read.
--
-- The result will behave exactly like the input 'Backend', except 'recv' will
-- return the input data before anythin else.
--
-- /TODO/ make more efficient
makeHybrid :: L.ByteString -> Backend -> IO Backend
makeHybrid buf0 Backend{..} = do
    bufBox <- newMVar buf0
    let recvWithInit n = do
            buf <- takeMVar bufBox
            let len = L.length buf
            if len < fromIntegral n
               then do
                    putMVar bufBox L.empty
                    (<>) (L.toStrict buf) <$> backendRecv (n - fromIntegral len)
               else let (go, stay) = L.splitAt (fromIntegral n) buf
                    in L.toStrict go <$ putMVar bufBox stay
    return $ Backend backendFlush backendClose backendSend recvWithInit

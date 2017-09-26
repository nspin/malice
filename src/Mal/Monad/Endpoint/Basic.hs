module Mal.Monad.Endpoint.Basic
    ( endpointChunk
    , endpointChunkRaw
    ) where

import Mal.Monad.Endpoint.Internal

import qualified Data.ByteString as B
import Data.ByteString.Builder

endpointChunk :: MonadEndpoint e m => m (Bool, Builder)
endpointChunk = f <$> endpointChunkRaw
  where
    f b = (B.null b, byteString b)

endpointChunkRaw :: MonadEndpoint e m => m B.ByteString
endpointChunkRaw = do
    b <- endpointGetState
    endpointPutState B.empty
    case B.length b of
        0 -> endpointRecv
        _ -> return b

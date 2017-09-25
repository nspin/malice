module Mal.Monad.Vertex.Serialize
    ( vertexPut
    , vertexPutPut
    ) where

import Mal.Monad.Vertex

import Data.Serialize

vertexPut :: (MonadVertex e m, Serialize a) => a -> m ()
vertexPut = vertexPutPut . put

vertexPutPut :: MonadVertex e m => Put -> m ()
vertexPutPut = vertexSendBuilder . execPut
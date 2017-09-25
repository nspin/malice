module Mal.Monad.Vertex.Serialize
    ( vertexPut
    , vertexPutPut
    ) where

import Mal.Monad.Vertex

import Data.Serialize

vertexPut :: (MonadVertex b e m, Serialize a) => a -> m ()
vertexPut = vertexPutPut . put

vertexPutPut :: MonadVertex b e m => Put -> m ()
vertexPutPut = vertexSendBuilder . execPut
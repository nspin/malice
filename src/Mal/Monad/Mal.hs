module Mal.Monad.Mal
    ( MonadMal(..)
    , MalT(..)

    , Vertices(..)
    , vertexFrom
    , vertexFromTo

    , runMalT
    , runMalT'
    , evalMalT
    , evalMalT'
    ) where

import Mal.Monad.Mal.Internal
import Mal.Monad.Eve
import Mal.Monad.Vertex

import Control.Monad.Reader
import qualified Data.ByteString as B


data Vertices m = Vertices
    { vertexAlice :: Vertex m
    , vertexBob :: Vertex m
    }

vertexFrom :: Side -> Vertices m -> Vertex m
vertexFrom Alice = vertexAlice
vertexFrom Bob = vertexBob

vertexFromTo :: Side -> Side -> Vertices m -> Vertex m
vertexFromTo x y = Vertex <$> (edgeIn . vertexFrom x) <*> (edgeOut . vertexFrom y)


runMalT :: MalT e m a -> Vertices m -> Buffers -> m (Either e a, Buffers)
runMalT m vs bufs = runEveT (runReaderT (getMalT m) sps) eps bufs
  where
    sps = Startpoints
        { toAlice = edgeOut $ vertexAlice vs
        , toBob = edgeOut $ vertexBob vs
        }
    eps = Endpoints
        { endpointFromAlice = edgeIn $ vertexAlice vs
        , endpointFromBob = edgeIn $ vertexBob vs
        }

runMalT' :: MalT e m a -> Vertices m -> m (Either e a, Buffers)
runMalT' m vs = runMalT m vs $ Buffers B.empty B.empty

evalMalT :: Monad m => MalT e m a -> Vertices m -> Buffers -> m (Either e a)
evalMalT = (fmap.fmap.fmap.fmap) fst runMalT

evalMalT' :: Monad m => MalT e m a -> Vertices m -> m (Either e a)
evalMalT' m vs = evalMalT m vs $ Buffers B.empty B.empty

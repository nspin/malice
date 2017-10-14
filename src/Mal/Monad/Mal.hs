module Mal.Monad.Mal
    ( HoistFromTo(..)
    , MonadMal(..)
    , MalT(..)

    , Vertices(..)
    , vertexFrom
    , vertexFromTo
    , endpointsOf

    , runMalT
    , runMalT'
    , evalMalT
    , evalMalT'
    ) where

import Mal.Monad.Mal.Internal
import Mal.Monad.Eve
import Mal.Monad.Vertex

import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as L


data Vertices m = Vertices
    { vertexAlice :: Vertex m
    , vertexBob :: Vertex m
    }

vertexFrom :: Side -> Vertices m -> Vertex m
vertexFrom Alice = vertexAlice
vertexFrom Bob = vertexBob

vertexFromTo :: Side -> Side -> Vertices m -> Vertex m
vertexFromTo x y = Vertex <$> (edgeIn . vertexFrom x) <*> (edgeOut . vertexFrom y)

endpointsOf :: Lens' (Vertices m) (Endpoints m)
endpointsOf = lens f g
  where
    f (Vertices alice bob) = Endpoints (edgeIn alice) (edgeIn bob)
    g (Vertices alice bob) (Endpoints al bo) = Vertices
        (Vertex al (edgeOut alice))
        (Vertex bo (edgeOut bob))


runMalT :: Monad m => MalT e m a -> Vertices m -> Unconsumed -> m (Either e a, Unconsumed)
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

runMalT' :: Monad m => MalT e m a -> Vertices m -> m (Either e a, Unconsumed)
runMalT' m vs = runMalT m vs $ Unconsumed L.empty L.empty

evalMalT :: Monad m => MalT e m a -> Vertices m -> Unconsumed -> m (Either e a)
evalMalT = (fmap.fmap.fmap.fmap) fst runMalT

evalMalT' :: Monad m => MalT e m a -> Vertices m -> m (Either e a)
evalMalT' m vs = evalMalT m vs $ Unconsumed L.empty L.empty

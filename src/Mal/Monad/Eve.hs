module Mal.Monad.Eve
    ( MonadEve(eveThrow, eveCatch)
    , Side(..)
    , HoistFrom(..)
    , EveT
    , runEveT

    , Endpoints(..)
    , endpointFrom
    , Unconsumed(..)
    ) where

import Mal.Monad.Endpoint.Internal
import Mal.Monad.Eve.Internal

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


data Unconsumed = Unconsumed
    { unconsumedFromAlice :: L.ByteString
    , unconsumedFromBob :: L.ByteString
    }

fromBuffers :: Buffers -> Unconsumed
fromBuffers (Buffers (Buffer _ alice) (Buffer _ bob)) = Unconsumed
    (L.fromChunks alice)
    (L.fromChunks bob)

toBuffers :: Unconsumed -> Buffers
toBuffers (Unconsumed alice bob) = Buffers
    (Buffer True (filter (not . B.null) (L.toChunks alice)))
    (Buffer True (filter (not . B.null) (L.toChunks bob)))


runEveT :: Monad m => EveT e m a -> Endpoints m -> Unconsumed -> m (Either e a, Unconsumed)
runEveT m eps init = (fmap.fmap) fromBuffers $
    runStateT
        (runExceptT (runReaderT (getEveT m) eps))
        (toBuffers init)
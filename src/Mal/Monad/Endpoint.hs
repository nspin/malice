module Mal.Monad.Endpoint
    ( MonadEndpoint(endpointThrow, endpointCatch)
    , EndpointT
    , runEndpointT
    , runEndpointT'
    , evalEndpointT
    , evalEndpointT'
    ) where

import Mal.Monad.Endpoint.Internal

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B

-- Many of these functions could have their arguments re-ordered for convenience,
-- but this way adheres to convention.

runEndpointT :: EndpointT e m a -> m B.ByteString -> B.ByteString -> m (Either e a, B.ByteString)
runEndpointT = fmap runStateT . fmap runExceptT . runReaderT . getEndpointT

runEndpointT' :: EndpointT e m a -> m B.ByteString -> m (Either e a, B.ByteString)
runEndpointT' endpoint recv = runEndpointT endpoint recv B.empty

evalEndpointT :: Monad m => EndpointT e m a -> m B.ByteString -> B.ByteString -> m (Either e a)
evalEndpointT = (fmap.fmap.fmap.fmap) fst runEndpointT

evalEndpointT' :: Monad m => EndpointT e m a -> m B.ByteString -> m (Either e a)
evalEndpointT' endpoint recv = evalEndpointT endpoint recv B.empty

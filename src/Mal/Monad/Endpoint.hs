module Mal.Monad.Endpoint
    ( MonadEndpoint(endpointThrow, endpointCatch)
    , EndpointT

    , endpointPush
    , endpointPop
    , endpointChunk

    , runEndpointT
    , runEndpointT'
    , evalEndpointT
    , evalEndpointT'
    ) where

import Mal.Monad.Endpoint.Internal

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


endpointHasMore :: MonadEndpoint e m => m Bool
endpointHasMore = endpointState $ (,) <$> _bufferMore <*> id

endpointNoMore :: MonadEndpoint e m => m ()
endpointNoMore = endpointState $ (,) () . set bufferMore False

endpointPush :: MonadEndpoint e m => B.ByteString -> m ()
endpointPush b = unless (B.null b) . endpointState $ bufferStack ((,) () . (:) b)

endpointPurePop :: MonadEndpoint e m => m (Maybe B.ByteString)
endpointPurePop = endpointState $ bufferStack f
  where
    f [] = (Nothing, [])
    f (b:bs) = (Just b, bs)

endpointPop :: MonadEndpoint e m => m (Maybe B.ByteString)
endpointPop = do
    mb <- endpointPurePop
    more <- endpointHasMore
    case (mb, more) of
        (Nothing, True) -> do
            b <- endpointRecv
            case B.length b of
                0 -> Nothing <$ endpointNoMore
                _ -> return $ Just b
        _ -> return mb

endpointChunk :: MonadEndpoint e m => m B.ByteString
endpointChunk = fromMaybe B.empty <$> endpointPop


-- Many of these functions could have their arguments re-ordered for convenience,
-- but this way adheres to convention.

runEndpointT :: Monad m => EndpointT e m a -> m B.ByteString -> L.ByteString -> m (Either e a, L.ByteString)
runEndpointT m recv init = (fmap.fmap) (L.fromChunks . view bufferStack) $
	runStateT
		(runExceptT (runReaderT (getEndpointT m) recv))
		(Buffer True (filter (not . B.null) (L.toChunks init)))

runEndpointT' :: Monad m => EndpointT e m a -> m B.ByteString -> m (Either e a, L.ByteString)
runEndpointT' endpoint recv = runEndpointT endpoint recv L.empty

evalEndpointT :: Monad m => EndpointT e m a -> m B.ByteString -> L.ByteString -> m (Either e a)
evalEndpointT = (fmap.fmap.fmap.fmap) fst runEndpointT

evalEndpointT' :: Monad m => EndpointT e m a -> m B.ByteString -> m (Either e a)
evalEndpointT' endpoint recv = evalEndpointT endpoint recv L.empty

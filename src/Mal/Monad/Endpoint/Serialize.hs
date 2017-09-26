{-# LANGUAGE FlexibleContexts #-}

module Mal.Monad.Endpoint.Serialize
    ( endpointGet
    , endpointGetWith
    , endpointGetKeep
    , endpointGetKeepWith
    ) where

import Mal.Monad.Endpoint.Internal

import Control.Monad.Writer
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Functor
import Data.Serialize


endpointGetKeep :: (MonadEndpoint String m, Serialize a) => m (a, Builder)
endpointGetKeep = endpointGetKeepWith get

endpointGet :: (MonadEndpoint String m, Serialize a) => m a
endpointGet = endpointGetWith get

endpointGetKeepWith :: MonadEndpoint String m => Get a -> m (a, Builder)
endpointGetKeepWith = runWriterT . go . runGetPartial
  where
    consumed orig suffix = B.take (B.length orig - B.length suffix) orig
    go f = do
        b' <- lift endpointGetState
        b <- case B.length b' of
                0 -> endpointRecv
                _ -> return b'
        let end i = tell (byteString (consumed b i)) >> lift (endpointPutState i)
        case f b of
            Fail err i -> end i >> endpointThrow err
            Done r   i -> end i $> r
            Partial f' ->  tell (byteString b) >> go f'

-- Probably more performant than @(fmap.fmap) fst endpointGetKeepWith@,
-- but it's possible that GHC is smart enough to make this unecessary.
endpointGetWith :: MonadEndpoint String m => Get a -> m a
endpointGetWith = go . runGetPartial
  where
    go f = do
        b' <- endpointGetState
        b <- case B.length b' of
                0 -> endpointRecv
                _ -> return b'
        case f b of
            Fail err i -> endpointPutState i >> endpointThrow err
            Done r   i -> endpointPutState i $> r
            Partial f' -> go f'

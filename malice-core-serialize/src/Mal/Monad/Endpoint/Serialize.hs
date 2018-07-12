{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mal.Monad.Endpoint.Serialize
    ( awaitGet
    ) where

import Mal.Monad.Endpoint

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Functor
import Data.Serialize


awaitGet :: (MonadEndpoint String m, Serialize a) => m a
awaitGet = await get


instance Awaitable String Get where

    await' = runWriterT . go . runGetPartial
      where
        consumed orig suffix = B.take (B.length orig - B.length suffix) orig
        go f = do
            b <- awaitChunk
            let end i = tell (byteString (consumed b i)) >> lift (endpointPush i)
            case f b of
                Fail err i -> end i >> raise err
                Done r   i -> end i $> r
                Partial f' -> tell (byteString b) >> go f'

    await = go . runGetPartial
      where
        go f = do
            b <- awaitChunk
            case f b of
                Fail err i -> endpointPush i >> raise err
                Done r   i -> endpointPush i $> r
                Partial f' -> go f'

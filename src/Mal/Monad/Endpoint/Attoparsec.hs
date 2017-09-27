{-# LANGUAGE FlexibleContexts #-}

module Mal.Monad.Endpoint.Attoparsec
    ( endpointParse
    , endpointParseKeep
    ) where

import Mal.Monad.Endpoint

import Control.Monad.Writer
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Functor
import Data.List

endpointParseKeep :: MonadEndpoint String m => Parser a -> m (a, Builder)
endpointParseKeep = runWriterT . go . parse
  where
    consumed orig suffix = B.take (B.length orig - B.length suffix) orig
    go f = do
        b <- endpointChunk
        let end i = tell (byteString (consumed b i)) >> lift (endpointPush i)
        case f b of
            Fail i []   err -> end i >> endpointThrow err
            Fail i ctxs err -> end i >> endpointThrow (intercalate " > " ctxs ++ ": " ++ err)
            Done i      r   -> end i $> r
            Partial f'      -> tell (byteString b) >> go f'

-- Probably more performant than @(fmap.fmap) fst endpointParseKeep@,
-- but it's possible that GHC is smart enough to make this unecessary.
endpointParse :: MonadEndpoint String m => Parser a -> m a
endpointParse = go . parse
  where
    go f = do
        b <- endpointChunk
        case f b of
            Fail i []   err -> endpointPush i >> endpointThrow err
            Fail i ctxs err -> endpointPush i >> endpointThrow (intercalate " > " ctxs ++ ": " ++ err)
            Done i      r   -> endpointPush i $> r
            Partial f'      -> go f'

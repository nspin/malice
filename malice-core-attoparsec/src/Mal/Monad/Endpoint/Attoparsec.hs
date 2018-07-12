{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mal.Monad.Endpoint.Attoparsec () where

import Mal.Monad.Endpoint

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Functor
import Data.List


instance Awaitable String Parser where

    await' = runWriterT . go . parse
      where
        consumed orig suffix = B.take (B.length orig - B.length suffix) orig
        go f = do
            b <- awaitChunk
            let end i = tell (byteString (consumed b i)) >> lift (endpointPush i)
            case f b of
                Fail i []   err -> end i >> raise err
                Fail i ctxs err -> end i >> raise (intercalate " > " ctxs ++ ": " ++ err)
                Done i      r   -> end i $> r
                Partial f'      -> tell (byteString b) >> go f'

    await = go . parse
      where
        go f = do
            b <- awaitChunk
            case f b of
                Fail i []   err -> endpointPush i >> raise err
                Fail i ctxs err -> endpointPush i >> raise (intercalate " > " ctxs ++ ": " ++ err)
                Done i      r   -> endpointPush i $> r
                Partial f'      -> go f'

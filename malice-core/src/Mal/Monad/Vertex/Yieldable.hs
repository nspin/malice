{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mal.Monad.Vertex.Yieldable
    ( Yieldable(..)
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Foldable


class Yieldable a where
    yieldWith :: Monad m => (B.ByteString -> m ()) -> a -> m ()

instance Yieldable B.ByteString where
    yieldWith = id

instance Yieldable L.ByteString where
    yieldWith f = traverse_ (yieldWith f) . L.toChunks

instance Yieldable Builder where
    yieldWith f = yieldWith f . toLazyByteString

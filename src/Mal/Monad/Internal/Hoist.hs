{-# LANGUAGE RankNTypes #-}

module Mal.Monad.Internal.Hoist
    ( substate
    ) where

import Control.Lens
import Control.Monad.State
import Data.Functor.Compose

substate :: Monad m => Lens' s s' -> StateT s' m a -> StateT s m a
substate l = StateT . fmap getCompose . l . fmap Compose . runStateT

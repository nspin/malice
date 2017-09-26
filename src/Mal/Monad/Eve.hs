module Mal.Monad.Eve
    ( Side(..)
    , HoistFrom(..)
    , MonadEve(eveThrow, eveCatch)
    , EveT
    , runEveT
    , Endpoints(..)
    , endpointFrom
    , Buffers(..)
    , bufferFrom
    ) where

import Mal.Monad.Eve.Internal

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

runEveT :: EveT e m a -> Endpoints m -> Buffers -> m (Either e a, Buffers)
runEveT = fmap runStateT . fmap runExceptT . runReaderT . getEveT

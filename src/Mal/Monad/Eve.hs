{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Eve
    ( Side(..)
    , Endpoints(..)
    , endpointFrom
    , Buffers(..)
    , bufferFrom
    , EveT
    , MonadEve(eveThrow, eveCatch)
    , runEveT
    , HoistEndpoint(..)
    ) where

import Mal.Monad.Eve.Internal
import Mal.Monad.Endpoint.Internal
import Mal.Monad.Vertex
import Mal.Monad.Internal.Hoist

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as B

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Trans.Identity as Identity (IdentityT(..), liftCatch)
import Control.Monad.Trans.List as List (ListT(..), liftCatch)
import Control.Monad.Trans.Maybe as Maybe (MaybeT(..), liftCatch)
import Control.Monad.Trans.Reader as Reader (ReaderT(..), liftCatch)
import Control.Monad.Trans.State.Lazy as LazyState (StateT(..), liftCatch)
import qualified Control.Monad.Trans.State.Strict as StrictState (StateT(..), liftCatch)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT(..), liftCatch)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT(..), liftCatch)


runEveT :: EveT e m a -> Endpoints m -> Buffers -> m (Either e a, Buffers)
runEveT = fmap runStateT . fmap runExceptT . runReaderT . getEveT


class (MonadEve e eve, MonadEndpoint e end) => HoistEndpoint e eve end | eve -> end where
    hoistFrom :: Side -> end a -> eve a

instance Monad m => HoistEndpoint e (EveT e m) (EndpointT e m) where
    hoistFrom side ep = EveT . ReaderT $ \eps -> ExceptT $
        substate (bufferFrom side) (runExceptT (runReaderT (getEndpointT ep) (endpointFrom side eps)))


instance HoistEndpoint e m u => HoistEndpoint e (IdentityT m) (IdentityT u) where
    hoistFrom side = IdentityT . hoistFrom side . runIdentityT

instance HoistEndpoint e m u => HoistEndpoint e (ListT m) (ListT u) where
    hoistFrom side = ListT . hoistFrom side . runListT

instance HoistEndpoint e m u => HoistEndpoint e (MaybeT m) (MaybeT u) where
    hoistFrom side = MaybeT . hoistFrom side . runMaybeT

instance HoistEndpoint e m u => HoistEndpoint e (ReaderT r m) (ReaderT r u) where
    hoistFrom side = ReaderT . fmap (hoistFrom side) . runReaderT

instance HoistEndpoint e m u => HoistEndpoint e (LazyState.StateT s m) (LazyState.StateT s u) where
    hoistFrom side = LazyState.StateT . (.) (hoistFrom side) . LazyState.runStateT

instance HoistEndpoint e m u => HoistEndpoint e (StrictState.StateT s m) (StrictState.StateT s u) where
    hoistFrom side = StrictState.StateT . (.) (hoistFrom side) . StrictState.runStateT

instance (Monoid w, HoistEndpoint e m u) => HoistEndpoint e (LazyWriter.WriterT w m) (LazyWriter.WriterT w u) where
    hoistFrom side = LazyWriter.WriterT . hoistFrom side . LazyWriter.runWriterT

instance (Monoid w, HoistEndpoint e m u) => HoistEndpoint e (StrictWriter.WriterT w m) (StrictWriter.WriterT w u) where
    hoistFrom side = StrictWriter.WriterT . hoistFrom side . StrictWriter.runWriterT

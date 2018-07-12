{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Eve.Internal
    ( MonadEve(..)
    , Side(..)
    , HoistFrom(..)
    , EveT(..)

    , Endpoints(..)
    , endpointFrom
    , Buffers(..)
    , bufferFrom
    ) where

import Mal.Monad.Endpoint.Internal
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
import Control.Monad.Catch
import Control.Monad.Logger

import Control.Monad.Trans.Identity as Identity (IdentityT(..), liftCatch)
import Control.Monad.Trans.List as List (ListT(..), liftCatch)
import Control.Monad.Trans.Maybe as Maybe (MaybeT(..), liftCatch)
import Control.Monad.Trans.Reader as Reader (ReaderT(..), liftCatch)
import Control.Monad.Trans.State.Lazy as LazyState (StateT(..), liftCatch)
import Control.Monad.Trans.State.Strict as StrictState (StateT(..), liftCatch)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT(..), liftCatch)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT(..), liftCatch)


data Side = Alice | Bob

data Endpoints m = Endpoints
    { endpointFromAlice :: m B.ByteString
    , endpointFromBob :: m B.ByteString
    }

endpointFrom :: Side -> Endpoints m -> m B.ByteString
endpointFrom Alice = endpointFromAlice
endpointFrom Bob = endpointFromBob

data Buffers = Buffers
    { _bufferFromAlice :: Buffer
    , _bufferFromBob :: Buffer
    }

makeLenses ''Buffers

bufferFrom :: Side -> Lens' Buffers Buffer
bufferFrom Alice = bufferFromAlice
bufferFrom Bob = bufferFromBob


newtype EveT e m a = EveT { getEveT :: ReaderT (Endpoints m) (ExceptT e (LazyState.StateT Buffers m)) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (EveT e) where
    lift = EveT . lift . lift . lift


class Monad m => HoistFrom m where
    type InnerEndpoint m :: * -> *
    hoistFrom :: Side -> InnerEndpoint m a -> m a

class (HoistFrom m, MonadEndpoint e (InnerEndpoint m)) => MonadEve e m | m -> e where
    eveRecvFrom :: Side -> m B.ByteString
    eveState :: (Buffers -> (a, Buffers)) -> m a
    eveThrow :: e -> m a
    eveCatch :: m a -> (e -> m a) -> m a


instance Monad m => HoistFrom (EveT e m) where
    type InnerEndpoint (EveT e m) = EndpointT e m
    hoistFrom side ep = EveT . ReaderT $ \eps ->
        ExceptT $ substate
            (bufferFrom side)
            (runExceptT (runReaderT (getEndpointT ep) (endpointFrom side eps)))

instance Monad m => MonadEve e (EveT e m) where
    eveRecvFrom side = EveT (ask >>= lift . lift . lift . endpointFrom side)
    eveState = EveT . state
    eveThrow = EveT . throwError
    eveCatch m f = EveT $ catchError (getEveT m) (getEveT . f)


-- EveT mtl lifts --

instance MonadError e m => MonadError e (EveT e m) where
    throwError = lift . throwError
    catchError b f = EveT . ReaderT $ \recv ->
        ExceptT $ catchError
            (runExceptT (runReaderT (getEveT b) recv))
            (runExceptT . flip runReaderT recv . getEveT . f)

instance MonadReader r m => MonadReader r (EveT e m) where
    ask = lift ask
    local f = EveT . ReaderT . (.) (local f) . runReaderT . getEveT

instance MonadState s m => MonadState s (EveT e m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (EveT e m) where
    tell = lift . tell
    listen = EveT . listen . getEveT
    pass = EveT . pass . getEveT

instance MonadThrow m => MonadThrow (EveT e m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (EveT e m) where
    catch m c = EveT $ catch (getEveT m) (getEveT . c)

instance MonadLogger m => MonadLogger (EveT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d


-- HoistFrom mtl lifts --

instance HoistFrom m => HoistFrom (IdentityT m) where
    type InnerEndpoint (IdentityT m) = IdentityT (InnerEndpoint m)
    hoistFrom side = IdentityT . hoistFrom side . runIdentityT

instance HoistFrom m => HoistFrom (ListT m) where
    type InnerEndpoint (ListT m) = ListT (InnerEndpoint m)
    hoistFrom side = ListT . hoistFrom side . runListT

instance HoistFrom m => HoistFrom (MaybeT m) where
    type InnerEndpoint (MaybeT m) = MaybeT (InnerEndpoint m)
    hoistFrom side = MaybeT . hoistFrom side . runMaybeT

instance HoistFrom m => HoistFrom (ExceptT e' m) where
    type InnerEndpoint (ExceptT e' m) = ExceptT e' (InnerEndpoint m)
    hoistFrom side = ExceptT . hoistFrom side . runExceptT

instance HoistFrom m => HoistFrom (ReaderT r m) where
    type InnerEndpoint (ReaderT r m) = ReaderT r (InnerEndpoint m)
    hoistFrom side = ReaderT . fmap (hoistFrom side) . runReaderT

instance HoistFrom m => HoistFrom (LazyState.StateT s m) where
    type InnerEndpoint (LazyState.StateT s m) = LazyState.StateT s (InnerEndpoint m)
    hoistFrom side = LazyState.StateT . (.) (hoistFrom side) . LazyState.runStateT

instance HoistFrom m => HoistFrom (StrictState.StateT s m) where
    type InnerEndpoint (StrictState.StateT s m) = StrictState.StateT s (InnerEndpoint m)
    hoistFrom side = StrictState.StateT . (.) (hoistFrom side) . StrictState.runStateT

instance (HoistFrom m, Monoid w) => HoistFrom (LazyWriter.WriterT w m) where
    type InnerEndpoint (LazyWriter.WriterT w m) = LazyWriter.WriterT w (InnerEndpoint m)
    hoistFrom side = LazyWriter.WriterT . hoistFrom side . LazyWriter.runWriterT

instance (HoistFrom m, Monoid w) => HoistFrom (StrictWriter.WriterT w m) where
    type InnerEndpoint (StrictWriter.WriterT w m) = StrictWriter.WriterT w (InnerEndpoint m)
    hoistFrom side = StrictWriter.WriterT . hoistFrom side . StrictWriter.runWriterT


-- MonadEve mtl lifts --

instance MonadEve e m => MonadEve e (ExceptT e' m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch m f = ExceptT $ eveCatch (runExceptT m) (runExceptT . f)

--

instance MonadEve e m => MonadEve e (IdentityT m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = Identity.liftCatch eveCatch

instance MonadEve e m => MonadEve e (ListT m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = List.liftCatch eveCatch

instance MonadEve e m => MonadEve e (MaybeT m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = Maybe.liftCatch eveCatch

instance MonadEve e m => MonadEve e (ReaderT t m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = Reader.liftCatch eveCatch

instance MonadEve e m => MonadEve e (LazyState.StateT s m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = LazyState.liftCatch eveCatch

instance MonadEve e m => MonadEve e (StrictState.StateT s m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = StrictState.liftCatch eveCatch

instance (MonadEve e m, Monoid w) => MonadEve e (LazyWriter.WriterT w m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = LazyWriter.liftCatch eveCatch

instance (MonadEve e m, Monoid w) => MonadEve e (StrictWriter.WriterT w m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = StrictWriter.liftCatch eveCatch

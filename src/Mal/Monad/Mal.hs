{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Mal
    ( MalT(..)
    , MonadMal(..)
    , Startpoints(..)
    , startpointTo
    , Vertices(..)
    , vertexFrom
    , vertexFromTo
    , runMalT
    , runMalT'
    , evalMalT
    , evalMalT'
    , HoistVertex(..)
    ) where

import Mal.Monad.Mal.Internal
import Mal.Monad.Eve.Internal
import Mal.Monad.Eve
import Mal.Monad.Endpoint.Internal
import Mal.Monad.Internal.Hoist
import Mal.Monad.Vertex

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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

import qualified Data.ByteString as B


data Vertices m = Vertices
    { vertexAlice :: Vertex m
    , vertexBob :: Vertex m
    }


runMalT :: MalT e m a -> Vertices m -> Buffers -> m (Either e a, Buffers)
runMalT m vs bufs = runEveT (runReaderT (getMalT m) sps) eps bufs
  where
    sps = Startpoints
        { toAlice = edgeOut $ vertexAlice vs
        , toBob = edgeOut $ vertexBob vs
        }
    eps = Endpoints
        { endpointFromAlice = edgeIn $ vertexAlice vs
        , endpointFromBob = edgeIn $ vertexBob vs
        }

runMalT' :: MalT e m a -> Vertices m -> m (Either e a, Buffers)
runMalT' m vs = runMalT m vs $ Buffers B.empty B.empty

evalMalT :: Monad m => MalT e m a -> Vertices m -> Buffers -> m (Either e a)
evalMalT = (fmap.fmap.fmap.fmap) fst runMalT

evalMalT' :: Monad m => MalT e m a -> Vertices m -> m (Either e a)
evalMalT' m vs = evalMalT m vs $ Buffers B.empty B.empty


instance Monad m => HoistEndpoint e (MalT e m) (EndpointT e m) where
    hoistFrom = (fmap . fmap) (MalT . lift) hoistFrom

class (MonadMal e mal, MonadVertex e vert) => HoistVertex e mal vert | mal -> vert where
    hoistFromTo :: Side -> Side -> vert a -> mal a

instance Monad m => HoistVertex e (MalT e m) (VertexT e m) where
    hoistFromTo sfrom sto vert = MalT . ReaderT $ \sps ->
        EveT . ReaderT $ \eps -> ExceptT $
            substate
                (bufferFrom sfrom)
                (runExceptT
                    (runReaderT
                        (getEndpointT (runReaderT (getVertexT vert) (startpointTo sto sps)))
                        (endpointFrom sfrom eps)))


instance HoistVertex e mal vert => HoistVertex e (IdentityT mal) (IdentityT vert) where
    hoistFromTo sfrom sto = IdentityT . hoistFromTo sfrom sto . runIdentityT

instance HoistVertex e mal vert => HoistVertex e (ListT mal) (ListT vert) where
    hoistFromTo sfrom sto = ListT . hoistFromTo sfrom sto . runListT

instance HoistVertex e mal vert => HoistVertex e (MaybeT mal) (MaybeT vert) where
    hoistFromTo sfrom sto = MaybeT . hoistFromTo sfrom sto . runMaybeT

instance HoistVertex e mal vert => HoistVertex e (ReaderT r mal) (ReaderT r vert) where
    hoistFromTo sfrom sto = ReaderT . fmap (hoistFromTo sfrom sto) . runReaderT

instance HoistVertex e mal vert => HoistVertex e (LazyState.StateT s mal) (LazyState.StateT s vert) where
    hoistFromTo sfrom sto = LazyState.StateT . (.) (hoistFromTo sfrom sto) . LazyState.runStateT

instance HoistVertex e mal vert => HoistVertex e (StrictState.StateT s mal) (StrictState.StateT s vert) where
    hoistFromTo sfrom sto = StrictState.StateT . (.) (hoistFromTo sfrom sto) . StrictState.runStateT

instance (Monoid w, HoistVertex e mal vert) => HoistVertex e (LazyWriter.WriterT w mal) (LazyWriter.WriterT w vert) where
    hoistFromTo sfrom sto = LazyWriter.WriterT . hoistFromTo sfrom sto . LazyWriter.runWriterT

instance (Monoid w, HoistVertex e mal vert) => HoistVertex e (StrictWriter.WriterT w mal) (StrictWriter.WriterT w vert) where
    hoistFromTo sfrom sto = StrictWriter.WriterT . hoistFromTo sfrom sto . StrictWriter.runWriterT


vertexFrom :: Side -> Vertices m -> Vertex m
vertexFrom Alice = vertexAlice
vertexFrom Bob = vertexBob

vertexFromTo :: Side -> Side -> Vertices m -> Vertex m
vertexFromTo x y = Vertex <$> (edgeIn . vertexFrom x) <*> (edgeOut . vertexFrom y)
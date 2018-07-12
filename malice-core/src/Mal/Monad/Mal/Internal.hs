{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Mal.Monad.Mal.Internal
    ( MalT(..)
    , HoistFromTo(..)
    , MonadMal(..)
    , Startpoints(..)
    , startpointTo
    ) where

import Mal.Monad.Endpoint
import Mal.Monad.Endpoint.Internal
import Mal.Monad.Eve
import Mal.Monad.Eve.Internal
import Mal.Monad.Internal.Hoist
import Mal.Monad.Internal.Hoist
import Mal.Monad.Vertex

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Functor

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Catch

import Control.Monad.Trans.Identity as Identity (IdentityT(..), runIdentityT)
import Control.Monad.Trans.List as List (ListT(..), runListT)
import Control.Monad.Trans.Maybe as Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except as Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader as Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.State.Lazy as LazyState (StateT(..), runStateT)
import Control.Monad.Trans.State.Strict as StrictState (StateT(..), runStateT)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT(..), runWriterT)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT(..), runWriterT)


data Startpoints m = Startpoints
    { toAlice :: B.ByteString -> m ()
    , toBob :: B.ByteString -> m ()
    }

startpointTo :: Side -> Startpoints m -> B.ByteString -> m ()
startpointTo Alice = toAlice
startpointTo Bob = toBob


newtype MalT e m a = MalT { getMalT :: ReaderT (Startpoints m) (EveT e m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (MalT e) where
    lift = MalT . lift . lift

instance Monad m => HoistFrom (MalT e m) where
    type InnerEndpoint (MalT e m) = InnerEndpoint (EveT e m)
    hoistFrom = (fmap . fmap) (MalT . lift) hoistFrom

instance Monad m => MonadEve e (MalT e m) where
    eveRecvFrom = MalT . lift . eveRecvFrom
    eveState = MalT . lift . eveState
    eveThrow = MalT . lift . eveThrow
    eveCatch m f = MalT . ReaderT $ \eps ->
        eveCatch
            (runReaderT (getMalT m) eps)
            (flip runReaderT eps . getMalT . f)


class Monad m => HoistFromTo m where
    type InnerVertex m :: * -> *
    hoistFromTo :: Side -> Side -> InnerVertex m a -> m a

instance Monad m => HoistFromTo (MalT e m) where
    type InnerVertex (MalT e m) = VertexT e m
    hoistFromTo sfrom sto vert = MalT . ReaderT $ \sps ->
        EveT . ReaderT $ \eps ->
            ExceptT $ substate
                (bufferFrom sfrom)
                (runExceptT
                    (runReaderT
                        (getEndpointT (runReaderT (getVertexT vert) (startpointTo sto sps)))
                        (endpointFrom sfrom eps)))


class (MonadEve e m, HoistFromTo m, MonadVertex e (InnerVertex m)) => MonadMal e m | m -> e where
    malSendTo :: Side -> B.ByteString -> m ()

instance Monad m => MonadMal e (MalT e m) where
    malSendTo side bs = MalT ask >>= \sps -> lift (startpointTo side sps bs)


-- MalT mtl lifts --

instance MonadError e m => MonadError e (MalT e m) where
    throwError = lift . throwError
    catchError m f = MalT $ catchError (getMalT m) (getMalT . f)

instance MonadReader r m => MonadReader r (MalT e m) where
    ask = lift ask
    local f = MalT . ReaderT . (.) (local f) . runReaderT . getMalT

instance MonadState s m => MonadState s (MalT e m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (MalT e m) where
    tell = lift . tell
    listen = MalT . listen . getMalT
    pass = MalT . pass . getMalT

instance MonadThrow m => MonadThrow (MalT e m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (MalT e m) where
    catch m c = MalT $ catch (getMalT m) (getMalT . c)


-- HoistFromTo mtl lifts --

instance HoistFromTo m => HoistFromTo (IdentityT m) where
    type InnerVertex (IdentityT m) = IdentityT (InnerVertex m)
    hoistFromTo sfrom sto = IdentityT . hoistFromTo sfrom sto . runIdentityT

instance HoistFromTo m => HoistFromTo (ListT m) where
    type InnerVertex (ListT m) = ListT (InnerVertex m)
    hoistFromTo sfrom sto = ListT . hoistFromTo sfrom sto . runListT

instance HoistFromTo m => HoistFromTo (MaybeT m) where
    type InnerVertex (MaybeT m) = MaybeT (InnerVertex m)
    hoistFromTo sfrom sto = MaybeT . hoistFromTo sfrom sto . runMaybeT

instance HoistFromTo m => HoistFromTo (ExceptT e' m) where
    type InnerVertex (ExceptT e' m) = ExceptT e' (InnerVertex m)
    hoistFromTo sfrom sto = ExceptT . hoistFromTo sfrom sto . runExceptT

instance HoistFromTo m => HoistFromTo (ReaderT r m) where
    type InnerVertex (ReaderT r m) = ReaderT r (InnerVertex m)
    hoistFromTo sfrom sto = ReaderT . fmap (hoistFromTo sfrom sto) . runReaderT

instance HoistFromTo m => HoistFromTo (LazyState.StateT s m) where
    type InnerVertex (LazyState.StateT s m) = LazyState.StateT s (InnerVertex m)
    hoistFromTo sfrom sto = LazyState.StateT . (.) (hoistFromTo sfrom sto) . LazyState.runStateT

instance HoistFromTo m => HoistFromTo (StrictState.StateT s m) where
    type InnerVertex (StrictState.StateT s m) = StrictState.StateT s (InnerVertex m)
    hoistFromTo sfrom sto = StrictState.StateT . (.) (hoistFromTo sfrom sto) . StrictState.runStateT

instance (HoistFromTo m, Monoid w) => HoistFromTo (LazyWriter.WriterT w m) where
    type InnerVertex (LazyWriter.WriterT w m) = LazyWriter.WriterT w (InnerVertex m)
    hoistFromTo sfrom sto = LazyWriter.WriterT . hoistFromTo sfrom sto . LazyWriter.runWriterT

instance (HoistFromTo m, Monoid w) => HoistFromTo (StrictWriter.WriterT w m) where
    type InnerVertex (StrictWriter.WriterT w m) = StrictWriter.WriterT w (InnerVertex m)
    hoistFromTo sfrom sto = StrictWriter.WriterT . hoistFromTo sfrom sto . StrictWriter.runWriterT


-- MonadVertex mtl lifts --

instance MonadMal e m => MonadMal e (IdentityT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ListT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (MaybeT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ExceptT e' m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ReaderT t m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (LazyState.StateT s m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (StrictState.StateT s m) where
    malSendTo side = lift . malSendTo side

instance (MonadMal e m, Monoid w) => MonadMal e (LazyWriter.WriterT w m) where
    malSendTo side = lift . malSendTo side

instance (MonadMal e m, Monoid w) => MonadMal e (StrictWriter.WriterT w m) where
    malSendTo side = lift . malSendTo side

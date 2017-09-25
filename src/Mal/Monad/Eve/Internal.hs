{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Eve.Internal
    ( Side(..)
    , Endpoints(..)
    , endpointFrom
    , Buffers(..)
    , bufferFrom
    , EveT(..)
    , MonadEve(..)
    , eveGetState
    , evePutState
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as B

import Control.Monad.Catch as C
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Writer
import Control.Monad.State

import Control.Monad.Trans.Identity as Identity (IdentityT, liftCatch)
import Control.Monad.Trans.List as List (ListT, liftCatch)
import Control.Monad.Trans.Maybe as Maybe (MaybeT, liftCatch)
import Control.Monad.Trans.Reader as Reader (ReaderT, liftCatch)
import Control.Monad.Trans.State.Lazy as LazyState (StateT, liftCatch)
import Control.Monad.Trans.State.Strict as StrictState (StateT, liftCatch)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT, liftCatch)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT, liftCatch)


data Side = Alice | Bob

data Endpoints m = Endpoints
    { endpointFromAlice :: m B.ByteString
    , endpointFromBob :: m B.ByteString
    }

endpointFrom :: Side -> Endpoints m -> m B.ByteString
endpointFrom Alice = endpointFromAlice
endpointFrom Bob = endpointFromBob

data Buffers = Buffers
    { bufferFromAlice :: B.ByteString
    , bufferFromBob :: B.ByteString
    }

bufferFrom :: Side -> Lens' Buffers B.ByteString
bufferFrom Alice = lens bufferFromAlice $ \buffs buf -> buffs { bufferFromAlice = buf }
bufferFrom Bob = lens bufferFromBob $ \buffs buf -> buffs { bufferFromBob = buf }


newtype EveT e m a = EveT { getEveT :: ReaderT (Endpoints m) (ExceptT e (LazyState.StateT Buffers m)) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (EveT e) where
    lift = EveT . lift . lift . lift


class Monad m => MonadEve e m | m -> e where
    eveRecvFrom :: Side -> m B.ByteString
    eveState :: (Buffers -> (a, Buffers)) -> m a
    eveThrow :: e -> m a
    eveCatch :: m a -> (e -> m a) -> m a

eveGetState :: MonadEve e m => m Buffers
eveGetState = eveState $ \s -> (s, s)

evePutState :: MonadEve e m => Buffers -> m ()
evePutState s = eveState $ const ((), s)


instance Monad m => MonadEve e (EveT e m) where
    eveRecvFrom side = EveT (ask >>= lift . lift . lift . endpointFrom side)
    eveState = EveT . state
    eveThrow = EveT . throwError
    eveCatch m f = EveT $ catchError (getEveT m) (getEveT . f)


-- EveT mtl lifts --

instance MonadError e m => MonadError e (EveT e m) where
    throwError = lift . throwError
    catchError b f = EveT . ReaderT $ \recv -> ExceptT $ catchError
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
    catch m c = EveT $ C.catch (getEveT m) (getEveT . c)

instance MonadLogger m => MonadLogger (EveT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d


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

instance (Monoid w, MonadEve e m) => MonadEve e (LazyWriter.WriterT w m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = LazyWriter.liftCatch eveCatch

instance (Monoid w, MonadEve e m) => MonadEve e (StrictWriter.WriterT w m) where
    eveRecvFrom = lift . eveRecvFrom
    eveState = lift . eveState
    eveThrow = lift . eveThrow
    eveCatch = StrictWriter.liftCatch eveCatch
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Mal.Internal
    ( MalT(..)
    , MonadMal(..)
    , Startpoints(..)
    , startpointTo
    ) where

import Mal.Monad.Eve
import Mal.Monad.Eve.Internal
import Mal.Monad.Endpoint
import Mal.Monad.Vertex
import Mal.Monad.Internal.Hoist

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Functor

import Control.Monad.Catch as C
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Logger

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

instance Monad m => MonadEve e (MalT e m) where
    eveRecvFrom = MalT . lift . eveRecvFrom
    eveState = MalT . lift . eveState
    eveThrow = MalT . lift . eveThrow
    eveCatch m f = MalT . ReaderT $ \eps ->
        eveCatch
            (runReaderT (getMalT m) eps)
            (flip runReaderT eps . getMalT . f) 


class MonadEve e m => MonadMal e m | m -> e where
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
    catch m c = MalT $ C.catch (getMalT m) (getMalT . c)

instance MonadLogger m => MonadLogger (MalT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d


-- MonadVertex mtl lifts --

instance MonadMal e m => MonadMal e (IdentityT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ListT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (MaybeT m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ExceptT e m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (ReaderT t m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (LazyState.StateT s m) where
    malSendTo side = lift . malSendTo side

instance MonadMal e m => MonadMal e (StrictState.StateT s m) where
    malSendTo side = lift . malSendTo side

instance (Monoid w, MonadMal e m) => MonadMal e (LazyWriter.WriterT w m) where
    malSendTo side = lift . malSendTo side

instance (Monoid w, MonadMal e m) => MonadMal e (StrictWriter.WriterT w m) where
    malSendTo side = lift . malSendTo side
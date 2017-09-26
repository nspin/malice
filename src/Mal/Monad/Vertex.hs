{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Mal.Monad.Vertex
    ( MonadVertex(..)
    , VertexT(..)

    , vertexSendBuilder
    , vertexPass
    , vertexCopy

    , Vertex(..)
    , runVertexT
    , runVertexT'
    , evalVertexT
    , evalVertexT'
    ) where

import Mal.Monad.Endpoint
import Mal.Monad.Endpoint.Basic
import Mal.Monad.Endpoint.Internal

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

import Control.Monad.Catch as C
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Logger

import Control.Monad.Trans.Identity as Identity (IdentityT(..), runIdentityT, mapIdentityT)
import Control.Monad.Trans.List as List (ListT(..), runListT, mapListT)
import Control.Monad.Trans.Maybe as Maybe (MaybeT(..), runMaybeT, mapMaybeT)
import Control.Monad.Trans.Except as Except (ExceptT(..), runExceptT, mapExceptT)
import Control.Monad.Trans.Reader as Reader (ReaderT(..), runReaderT, mapReaderT)
import Control.Monad.Trans.State.Lazy as LazyState (StateT(..), runStateT, mapStateT)
import Control.Monad.Trans.State.Strict as StrictState (StateT(..), runStateT, mapStateT)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT(..), runWriterT, mapWriterT)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT(..), runWriterT, mapWriterT)


newtype VertexT e m a = VertexT { getVertexT :: ReaderT (B.ByteString -> m ()) (EndpointT e m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (VertexT e) where
    lift = VertexT . lift . lift

instance Monad m => MonadEndpoint e (VertexT e m) where
    endpointRecv = VertexT $ lift endpointRecv
    endpointState = VertexT . lift . endpointState
    endpointThrow = VertexT . lift . endpointThrow
    endpointCatch m f = VertexT . ReaderT $ \send ->
        endpointCatch
            (runReaderT (getVertexT m) send)
            (flip runReaderT send . getVertexT . f)


class MonadEndpoint e m => MonadVertex e m | m -> e where
    vertexSend :: B.ByteString -> m ()

instance Monad m => MonadVertex e (VertexT e m) where
    vertexSend bs = VertexT ask >>= \send -> lift (send bs)


vertexSendBuilder :: MonadVertex e m => Builder -> m ()
vertexSendBuilder = void . traverse vertexSend . L.toChunks . toLazyByteString

vertexPass :: MonadVertex e m => m (a, Builder) -> m a
vertexPass = (=<<) $ uncurry (<$) . fmap vertexSendBuilder

vertexCopy :: MonadVertex e m => m ()
vertexCopy = vertexPass endpointChunk >>= flip unless vertexCopy


data Vertex m = Vertex
    { edgeIn :: m B.ByteString
    , edgeOut :: B.ByteString -> m ()
    }

-- Many of these functions could have their arguments re-ordered for convenience,
-- but this way adheres to convention.

runVertexT :: VertexT e m a -> Vertex m -> B.ByteString -> m (Either e a, B.ByteString)
runVertexT m (Vertex ein eout) = runEndpointT (runReaderT (getVertexT m) eout) ein

runVertexT' :: VertexT e m a -> Vertex m -> m (Either e a, B.ByteString)
runVertexT' m v = runVertexT m v B.empty

evalVertexT :: Monad m => VertexT e m a -> Vertex m -> B.ByteString -> m (Either e a)
evalVertexT = (fmap.fmap.fmap.fmap) fst runVertexT

evalVertexT' :: Monad m => VertexT e m a -> Vertex m -> m (Either e a)
evalVertexT' vertex recv = evalVertexT vertex recv B.empty


-- VertexT mtl lifts --

instance MonadError e m => MonadError e (VertexT e m) where
    throwError = lift . throwError
    catchError m f = VertexT $ catchError (getVertexT m) (getVertexT . f)

instance MonadReader r m => MonadReader r (VertexT e m) where
    ask = lift ask
    local f = VertexT . ReaderT . (.) (local f) . runReaderT . getVertexT

instance MonadState s m => MonadState s (VertexT e m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (VertexT e m) where
    tell = lift . tell
    listen = VertexT . listen . getVertexT
    pass = VertexT . pass . getVertexT

instance MonadThrow m => MonadThrow (VertexT e m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (VertexT e m) where
    catch m c = VertexT $ C.catch (getVertexT m) (getVertexT . c)

instance MonadLogger m => MonadLogger (VertexT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d


-- MonadVertex mtl lifts --

instance MonadVertex e m => MonadVertex e (IdentityT m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (ListT m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (MaybeT m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (ExceptT e m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (ReaderT t m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (LazyState.StateT s m) where
    vertexSend = lift . vertexSend

instance MonadVertex e m => MonadVertex e (StrictState.StateT s m) where
    vertexSend = lift . vertexSend

instance (Monoid w, MonadVertex e m) => MonadVertex e (LazyWriter.WriterT w m) where
    vertexSend = lift . vertexSend

instance (Monoid w, MonadVertex e m) => MonadVertex e (StrictWriter.WriterT w m) where
    vertexSend = lift . vertexSend
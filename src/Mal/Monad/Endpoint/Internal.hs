{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Mal.Monad.Endpoint.Internal
    ( EndpointT(..)
    , MonadEndpoint(..)
    , endpointGetState
    , endpointPutState
    , endpointUse
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception (assert)
import qualified Data.ByteString as B

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Logger

import Control.Monad.Trans.Identity as Identity (IdentityT, liftCatch)
import Control.Monad.Trans.List as List (ListT, liftCatch)
import Control.Monad.Trans.Maybe as Maybe (MaybeT, liftCatch)
import Control.Monad.Trans.Except as Except (ExceptT)
import Control.Monad.Trans.Reader as Reader (ReaderT, liftCatch)
import Control.Monad.Trans.State.Lazy as LazyState (StateT, liftCatch)
import Control.Monad.Trans.State.Strict as StrictState (StateT, liftCatch)
import Control.Monad.Trans.Writer.Lazy as LazyWriter (WriterT, liftCatch)
import Control.Monad.Trans.Writer.Strict as StrictWriter (WriterT, liftCatch)


newtype EndpointT e m a = EndpointT { getEndpointT :: ReaderT (m B.ByteString) (ExceptT e (LazyState.StateT B.ByteString m)) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (EndpointT e) where
    lift = EndpointT . lift . lift . lift


class Monad m => MonadEndpoint e m | m -> e where
    endpointRecv :: m B.ByteString
    endpointState :: (B.ByteString -> (a, B.ByteString)) -> m a
    endpointThrow :: e -> m a
    endpointCatch :: m a -> (e -> m a) -> m a

instance Monad m => MonadEndpoint e (EndpointT e m) where
    endpointRecv =  EndpointT ask >>= lift
    endpointState = EndpointT . state
    endpointThrow = EndpointT . throwError
    endpointCatch m f = EndpointT $ catchError (getEndpointT m) (getEndpointT . f)


endpointGetState :: MonadEndpoint e m => m B.ByteString
endpointGetState = endpointState $ \s -> (s, s)

endpointPutState :: MonadEndpoint e m => B.ByteString -> m ()
endpointPutState s = endpointState $ const ((), s)

endpointUse :: MonadEndpoint e m => (B.ByteString -> (a, Int)) -> m a
endpointUse f = do
    b' <- endpointGetState
    b <- case B.length b' of
            0 -> endpointRecv
            _ -> return b'
    let (a, n) = f b
    endpointPutState $ assert (n <= B.length b) (B.drop n b)
    return a


-- EndpointT mtl lifts --

instance MonadError e m => MonadError e (EndpointT e m) where
    throwError = lift . throwError
    catchError m f = EndpointT . ReaderT $ \recv ->
        ExceptT $ catchError
            (runExceptT (runReaderT (getEndpointT m) recv))
            (runExceptT . flip runReaderT recv . getEndpointT . f)

instance MonadReader r m => MonadReader r (EndpointT e m) where
    ask = lift ask
    local f = EndpointT . ReaderT . (.) (local f) . runReaderT . getEndpointT

instance MonadState s m => MonadState s (EndpointT e m) where
    get = lift get
    put = lift . put

instance MonadWriter w m => MonadWriter w (EndpointT e m) where
    tell = lift . tell
    listen = EndpointT . listen . getEndpointT
    pass = EndpointT . pass . getEndpointT

instance MonadThrow m => MonadThrow (EndpointT e m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (EndpointT e m) where
    catch m c = EndpointT $ catch (getEndpointT m) (getEndpointT . c)

instance MonadLogger m => MonadLogger (EndpointT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d


-- MonadEndpoint mtl lifts --

instance MonadEndpoint e m => MonadEndpoint e (ExceptT e' m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch m f = ExceptT $ endpointCatch (runExceptT m) (runExceptT . f)

--

instance MonadEndpoint e m => MonadEndpoint e (IdentityT m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = Identity.liftCatch endpointCatch

instance MonadEndpoint e m => MonadEndpoint e (ListT m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = List.liftCatch endpointCatch

instance MonadEndpoint e m => MonadEndpoint e (MaybeT m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = Maybe.liftCatch endpointCatch

instance MonadEndpoint e m => MonadEndpoint e (ReaderT t m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = Reader.liftCatch endpointCatch

instance MonadEndpoint e m => MonadEndpoint e (LazyState.StateT s m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = LazyState.liftCatch endpointCatch

instance MonadEndpoint e m => MonadEndpoint e (StrictState.StateT s m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = StrictState.liftCatch endpointCatch

instance (Monoid w, MonadEndpoint e m) => MonadEndpoint e (LazyWriter.WriterT w m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = LazyWriter.liftCatch endpointCatch

instance (Monoid w, MonadEndpoint e m) => MonadEndpoint e (StrictWriter.WriterT w m) where
    endpointRecv = lift endpointRecv
    endpointState = lift . endpointState
    endpointThrow = lift . endpointThrow
    endpointCatch = StrictWriter.liftCatch endpointCatch

module Mal.Monad.Instances.Logger () where

import Mal.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Class

instance MonadLogger m => MonadLogger (EndpointT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

instance MonadLogger m => MonadLogger (VertexT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

instance MonadLogger m => MonadLogger (EveT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

instance MonadLogger m => MonadLogger (MalT e m) where
    monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

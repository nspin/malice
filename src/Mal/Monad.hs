{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Mal.Monad
    ( module Mal.Monad.Endpoint
    , module Mal.Monad.Vertex
    , module Mal.Monad.Eve
    , module Mal.Monad.Mal

    , module Mal.Monad.Endpoint.Basic
    , module Mal.Monad.Endpoint.Serialize
    , module Mal.Monad.Endpoint.Attoparsec

    , module Mal.Monad.Vertex.Serialize

    , EveBoth
    , MalBoth
    , EveInner
    , MalInner
    ) where

import Mal.Monad.Endpoint
import Mal.Monad.Vertex
import Mal.Monad.Eve
import Mal.Monad.Mal

import Mal.Monad.Endpoint.Basic
import Mal.Monad.Endpoint.Serialize
import Mal.Monad.Endpoint.Attoparsec

import Mal.Monad.Vertex.Serialize

import GHC.Exts (Constraint)

type EveBoth e m = (MonadEve e m, MonadEndpoint e (InnerEndpoint m))
type MalBoth e m = (MonadMal e m, MonadVertex e (InnerVertex m))

type EveInner (c :: (* -> *) -> Constraint) m = (c m, c (InnerEndpoint m))
type MalInner (c :: (* -> *) -> Constraint) m = (c m, c (InnerVertex m), c (InnerEndpoint m))

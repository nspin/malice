{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

module Mal.Monad
    ( module Mal.Monad.Endpoint
    , module Mal.Monad.Vertex
    , module Mal.Monad.Eve
    , module Mal.Monad.Mal

    , module Mal.Monad.Endpoint.Serialize
    , module Mal.Monad.Endpoint.Attoparsec

    , module Mal.Monad.Vertex.Serialize

    , EveInner
    , MalInner
    , EveAll
    , MalAll
    ) where

import Mal.Monad.Endpoint
import Mal.Monad.Vertex
import Mal.Monad.Eve
import Mal.Monad.Mal

import Mal.Monad.Endpoint.Serialize
import Mal.Monad.Endpoint.Attoparsec

import Mal.Monad.Vertex.Serialize

import GHC.Exts (Constraint)

type EveInner (c :: (* -> *) -> Constraint) m = c (InnerEndpoint m)
type MalInner (c :: (* -> *) -> Constraint) m = c (InnerVertex m)

type EveAll (c :: (* -> *) -> Constraint) m = (c m, c (InnerEndpoint m))
type MalAll (c :: (* -> *) -> Constraint) m = (c m, c (InnerVertex m), c (InnerEndpoint m))

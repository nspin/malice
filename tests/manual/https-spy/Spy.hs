{-# LANGUAGE FlexibleContexts #-}

module Spy (spy) where

import Mal.Monad
import Mal.Protocol.HTTP

import Control.Monad.Logger
import Data.Attoparsec.ByteString.Char8 hiding (option, Parser)

spy :: (MonadEve String m, EveAll MonadLogger m) => m ()
spy = do
    hoistFrom Alice request
    hoistFrom Bob response
    spy

request :: (MonadEndpoint String m, MonadLogger m) => m ()
request = do
    rline <- await requestLine
    hdrs <- await headers
    case requestBodyLength hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> requestBody info f
  where
    f bs = return ()

response :: (MonadEndpoint String m, MonadLogger m) => m ()
response = do
    rline <- await (statusLine <?> "status line")
    hdrs <- await headers
    case requestBodyLength hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> requestBody info f
  where
    f bs = return ()

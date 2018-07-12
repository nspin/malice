{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Spy (spy) where

import Prelude hiding (takeWhile)

import Mal.Monad
import Mal.Monad.Instances.Logger
import Mal.Protocol.HTTP
import Mal.Extra.Binascii

import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Data.Attoparsec.ByteString.Char8
import Data.Monoid
import qualified Data.Attoparsec.Internal.Types as I
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

spy :: (MonadEve String m, EveAll MonadLogger m) => m ()
spy = do
    hoistFrom Alice request
    hoistFrom Bob response
    done <- hoistFrom Alice $ await (atEnd :: I.Parser B.ByteString Bool)
    unless done spy

request :: (MonadEndpoint String m, MonadLogger m) => m ()
request = do
    await $ takeWhile (`elem` ("\r\n" :: String))
    rline <- await $ (requestLine <?> "xA")
    hdrs <- await $ (headers <?> "xB")
    case requestBodyLength hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> requestBody info f
  where
    f bs = return ()

response :: (MonadEndpoint String m, MonadLogger m) => m ()
response = do
    await $ takeWhile (`elem` ("\r\n" :: String))
    rline <- await $ (statusLine <?> "xC")
    hdrs <- await $ (headers <?> "xD")
    case requestBodyLength hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> requestBody info f
  where
    f bs = return ()

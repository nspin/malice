{-# LANGUAGE FlexibleContexts #-}

module Mal.Protocol.HTTP
    ( httpVersion
    , requestLine
    , statusLine
    , headers
    , requestBody
    , RequestBodyLength(..)
    , requestBodyLength
    ) where

import Mal.Monad

import Prelude hiding (take, takeWhile)
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.CaseInsensitive (mk, original)
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import Network.HTTP.Types


httpVersion :: Parser HttpVersion
httpVersion = "HTTP/" *> (HttpVersion <$> decimal <*> (char '.' *> decimal))

requestLine :: Parser (Method, B.ByteString, HttpVersion)
requestLine = do
    meth <- takeTill (== ' ')
    char ' '
    uri <- takeTill (== ' ')
    char ' '
    ver <- httpVersion
    crlf
    return (meth, uri, ver)

statusLine :: Parser (HttpVersion, Integer, B.ByteString)
statusLine = do
    ver <- httpVersion
    char ' '
    stat <- decimal
    char ' '
    reason <- takeTill (== '\r') <* crlf
    return (ver, stat, reason) 

headers :: Parser [Header]
headers = flip manyTill crlf $ do
    key <- takeTill (inClass ":\r")
    option () . void $ char ':' *> takeWhile (inClass " \t")
    val <- takeTill (== '\r') <* crlf
    return (mk key, val)

data RequestBodyLength = ChunkedBody | KnownLength Int deriving Show

requestBody :: MonadEndpoint String m => RequestBodyLength -> (B.ByteString -> m ()) -> m ()
requestBody (KnownLength len) f = go len
  where
    go n = do
        mbs <- endpointPop
        case mbs of
            Nothing -> return ()
            Just bs -> f bs >> go (n - B.length bs)
requestBody ChunkedBody f = new
  where
    new = do
        n <- await $ ((hexadecimal <* crlf) <?> "foo")
        unless (n == 0) (go n)
    go n = do
        mbs <- endpointPop
        case mbs of
            Nothing -> await crlf >> new
            Just bs -> do
                let (x, y) = B.splitAt n bs
                endpointPush y
                f x
                case n - B.length x of
                    0 -> await crlf >> new
                    n' -> go n'

requestBodyLength :: [Header] -> Maybe RequestBodyLength
requestBodyLength = asum . map f
  where
    f (k, v)
        | k == "Transfer-Encoding" = if v == "chunked"
            then Just ChunkedBody
            else Nothing
        | k == "Content-Length" = case parseOnly decimal v of
            Left _ -> Nothing
            Right n -> Just (KnownLength n)
        | otherwise = Nothing

crlf :: Parser ()
crlf = void "\r\n"

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FlipImages
    ( flipImages
    ) where

import Mal.Monad

import Prelude hiding (take)
import Codec.Picture.Extra
import Codec.Picture.Jpg
import Codec.Picture.Png
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Bool
import Data.ByteString.Builder
import Data.CaseInsensitive (mk, original)
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import Network.HTTP.Types
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L


flipImages :: forall m. (MonadIO m, MonadLogger m) => MalT String m ()
flipImages = do
    hoistFromTo Alice Bob $ (request :: VertexT String m ())
    hoistFromTo Bob Alice $ (response :: VertexT String m ())

request :: (MonadIO m, MonadLogger m, MonadVertex b String m) => m ()
request = do
    req <- vertexPass . endpointParseKeep $ manyTill anyChar endOfLine
    ohdrs <- endpointParse headers
    vertexSendBuilder . buildHeaders $ clientHeaders ohdrs
    case bodyInfo ohdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> void . vertexPass . endpointParseKeep $ body info

response :: (MonadIO m, MonadLogger m, MonadVertex b String m) => m ()
response = do
    status <- vertexPass . endpointParseKeep $ manyTill anyChar endOfLine
    (ohdrs, obs) <- endpointParseKeep headers
    case imageType ohdrs of
        Nothing -> vertexSendBuilder obs >> vertexCopy
        Just it -> do
            info <- maybe (endpointThrow $ "no body info " ++ status ++ show ohdrs) return (bodyInfo ohdrs)
            vertexSendBuilder . buildHeaders $
                removeHeaders ["Transfer-Encoding", "Content-Length"] ohdrs
                    ++ [("Transfer-Encoding", "chunked")] 
            b <- endpointParse $ body info
            let eb' = flipImage it (L.toStrict (toLazyByteString b))
            case eb' of
                Left err -> endpointThrow err
                Right b' -> vertexSendBuilder $ buildChunks b'


data BodyInfo = Length Int | Chunked deriving Show

data ImageType = JPG | PNG deriving Show


headers :: Parser [Header]
headers = manyTill header endOfLine
  where
    header = f <$> manyTill anyWord8 (string ": ") <*> manyTill anyWord8 endOfLine
    f k v = (mk (B.pack k), B.pack v)

body :: BodyInfo -> Parser Builder
body (Length n) = byteString <$> take n
body Chunked = go mempty
  where
    go acc = do
        n <- hexadecimal <* endOfLine
        case n of
            0 -> acc <$ endOfLine
            _ -> take n <* endOfLine >>= (go . (<>) acc . byteString)


buildHeaders :: [Header] -> Builder
buildHeaders hs = foldMap f hs <> byteString "\r\n"
  where
    f (k, v) = byteString (original k)
            <> byteString ": "
            <> byteString v
            <> byteString "\r\n"

buildChunks :: L.ByteString -> Builder
buildChunks bs =
    if L.length bs == 0
    then byteString "0\r\n\r\n"
    else
        let n = min 0x2000 (L.length bs)
            (x, y) = L.splitAt n bs
        in byteString (C.pack (showHex n ""))
            <> byteString "\r\n"
            <> lazyByteString x
            <> byteString "\r\n"
            <> buildChunks y


clientHeaders :: [Header] -> [Header]
clientHeaders hs = removeHeaders ["Connection", "Accept-Encoding"] hs
    ++ [("Connection", "close"), ("Accept-Encoding", "")]

removeHeaders :: [HeaderName] -> [Header] -> [Header]
removeHeaders = filter . flip fmap fst . fmap and .  traverse (/=)

bodyInfo :: [Header] -> Maybe BodyInfo
bodyInfo = asum . map f
  where
    f (k, v)
        | k == "Transfer-Encoding" = if v == "chunked" then Just Chunked else Nothing
        | k == "Content-Length" = either (const Nothing) (Just . Length) $ parseOnly decimal v
        | otherwise = Nothing

imageType :: [Header] -> Maybe ImageType
imageType = (>=>) (lookup "Content-Type") $ (<|>)
    <$> (f JPG <$> (==) "image/jpeg")
    <*> (f PNG <$> (==) "image/png")
  where
    f x = bool Nothing (Just x)


flipImage :: ImageType -> B.ByteString -> Either String L.ByteString
flipImage JPG bs = case decodeJpeg bs of
    (Right (ImageYCbCr8 img)) -> Right $ encodeJpeg $ flipVertically img
    Right _ -> Left "unrecognized jpeg contents"
    Left err -> Left err
flipImage PNG bs = case decodePng bs of
    Right dimg -> encodeDynamicPng $ flipDynamic dimg
    Left err -> Left err

flipDynamic :: DynamicImage -> DynamicImage
flipDynamic dimg = case dimg of
    ImageY8 img -> ImageY8 $ flipVertically img
    ImageY16 img -> ImageY16 $ flipVertically img
    ImageYF img -> ImageYF $ flipVertically img
    ImageYA8 img -> ImageYA8 $ flipVertically img
    ImageYA16 img -> ImageYA16 $ flipVertically img
    ImageRGB8 img -> ImageRGB8 $ flipVertically img
    ImageRGB16 img -> ImageRGB16 $ flipVertically img
    ImageRGBF img -> ImageRGBF $ flipVertically img
    ImageRGBA8 img -> ImageRGBA8 $ flipVertically img
    ImageRGBA16 img -> ImageRGBA16 $ flipVertically img
    ImageYCbCr8 img -> ImageYCbCr8 $ flipVertically img
    ImageCMYK8 img -> ImageCMYK8 $ flipVertically img
    ImageCMYK16 img -> ImageCMYK16 $ flipVertically img

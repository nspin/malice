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
import Data.Attoparsec.ByteString hiding (word8)
import Data.Attoparsec.ByteString.Char8
import Data.Bool
import Data.ByteString.Builder
import Data.CaseInsensitive (mk, original)
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


flipImages :: (MonadIO v, MonadLogger v, HoistVertex String m v) => m ()
flipImages = do
    hoistFromTo Alice Bob request
    hoistFromTo Bob Alice response

request :: (MonadIO m, MonadLogger m, MonadVertex String m) => m ()
request = do
    req <- vertexPass . endpointParseKeep $ manyTill anyChar endOfLine
    hdrs <- endpointParse headers
    vertexSendBuilder . buildHeaders $
        removeHeaders ["Connection", "Accept-Encoding"] hdrs
            ++ [("Connection", "close"), ("Accept-Encoding", "")]
    case bodyInfo hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> void . vertexPass . endpointParseKeep $ body info

response :: (MonadIO m, MonadLogger m, MonadVertex String m) => m ()
response = do
    status <- vertexPass . endpointParseKeep $ manyTill anyChar endOfLine
    (hdrs, hbs) <- endpointParseKeep headers
    case imageType hdrs of
        Nothing -> vertexSendBuilder hbs >> vertexCopy
        Just ityp -> do
            vertexSendBuilder . buildHeaders $
                removeHeaders ["Transfer-Encoding", "Content-Length"] hdrs
                    ++ [("Transfer-Encoding", "chunked")] 
            b <- endpointParse $ case bodyInfo hdrs of
                    Nothing -> foldMap word8 <$> manyTill anyWord8 endOfInput
                    Just info -> body info
            case flipImage ityp (L.toStrict (toLazyByteString b)) of
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
buildHeaders hs = foldMap f hs <> newline
  where
    f (k, v) = byteString (original k)
        <> byteString ": "
        <> byteString v
        <> newline

buildChunks :: L.ByteString -> Builder
buildChunks bs =
    if L.length bs == 0
    then byteString "0\r\n\r\n"
    else wordHex (fromIntegral n)
        <> newline
        <> lazyByteString x
        <> newline
        <> buildChunks y
  where
    n = min 0x2000 (L.length bs)
    (x, y) = L.splitAt n bs

newline :: Builder
newline = byteString "\r\n"


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

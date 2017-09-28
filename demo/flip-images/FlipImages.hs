{-# LANGUAGE FlexibleContexts #-}

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
import Data.Attoparsec.ByteString hiding (word8)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.CaseInsensitive (mk, original)
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import Network.HTTP.Types

-- Mal action to flip images. HTTP is synchronous, so we just handle the
-- request then the response in order. If logging or IO were necessary, they
-- could be added to the type signature using the constraints found in
-- 'Mal.Monad'. For example, 'MalAll MonadLogger m' would allow you to do
-- logging in both the Mal context and the hoisted Vertex contexts.
flipImages :: MonadMal String m => m ()
flipImages = do
    hoistFromTo Alice Bob request
    hoistFromTo Bob Alice response

-- Modify the request from Alice to Bob to make sure that the response is easy
-- to mess with, because we are lazy. In real life, this would compromise
-- Mallory's position. In particular, we make sure the response is not
-- compressed and that the connection is only used for one request.
request :: MonadVertex String m => m ()
request = do
    -- Pass along request line
    req <- forward $ manyTill anyChar endOfLine
    -- Parse headers
    hdrs <- await headers
    -- Send modified headers
    yield . buildHeaders $
        removeHeaders ["Connection", "Accept-Encoding"] hdrs
            ++ [("Connection", "close"), ("Accept-Encoding", "")]
    -- Forward body according to its tranfer encoding
    case bodyInfo hdrs of
        Nothing -> return () -- assume method doesn't allow request body
        Just info -> forward_ $ body info

-- If response body is a JPEG or PNG, flip it.
response :: MonadVertex String m => m ()
response = do
    -- Pass along status line
    status <- forward $ manyTill anyChar endOfLine
    -- Parse headers, and keep original bytes
    (hdrs, hb) <- await' headers
    case imageType hdrs of
        -- No image, just send original headers and proxy the rest of the response
        Nothing -> yield hb >> proxy
        Just ityp -> do
            -- Make sure headers indicate chunked transfer encoding, and send them
            yield . buildHeaders $
                removeHeaders ["Transfer-Encoding", "Content-Length"] hdrs
                    ++ [("Transfer-Encoding", "chunked")]
            -- Parse body according to original transfer encoding
            b <- await $ case bodyInfo hdrs of
                    Nothing -> foldMap word8 <$> manyTill anyWord8 endOfInput
                    Just info -> body info
            -- Flip the image and send it
            case flipImage ityp (L.toStrict (toLazyByteString b)) of
                Left err -> raise err
                Right b' -> yield $ buildChunks b'


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
    f (k, v) = byteString (original k) <> ": " <> byteString v <> newline

buildChunks :: L.ByteString -> Builder
buildChunks b =
    if L.null b
    then "0" <> newline <> newline
    else wordHex (fromIntegral n)
        <> newline
        <> lazyByteString x
        <> newline
        <> buildChunks y
  where
    n = min 0x2000 (L.length b)
    (x, y) = L.splitAt n b

newline :: Builder
newline = "\r\n"


removeHeaders :: [HeaderName] -> [Header] -> [Header]
removeHeaders = filter . (. fst) . (and .) .  traverse (/=)

bodyInfo :: [Header] -> Maybe BodyInfo
bodyInfo = asum . map f
  where
    f (k, v)
        | k == "Transfer-Encoding" = if v == "chunked"
            then Just Chunked
            else Nothing
        | k == "Content-Length" = case parseOnly decimal v of
            Left _ -> Nothing
            Right n -> Just (Length n)
        | otherwise = Nothing

imageType :: [Header] -> Maybe ImageType
imageType = lookup "Content-Type" >=> f
  where
    f v | v == "image/jpeg" = Just JPG
        | v == "image/png" = Just PNG
        | otherwise = Nothing


flipImage :: ImageType -> B.ByteString -> Either String L.ByteString
flipImage JPG b = case decodeJpeg b of
    (Right (ImageYCbCr8 img)) -> Right . encodeJpeg $ flipVertically img
    Right _ -> Left "unrecognized jpeg contents"
    Left err -> Left err
flipImage PNG b = case decodePng b of
    Right dimg -> encodeDynamicPng $ flipDynamic dimg
    Left err -> Left err

flipDynamic :: DynamicImage -> DynamicImage
flipDynamic dimg = case dimg of
    ImageY8     x -> ImageY8     (flipVertically x)
    ImageY16    x -> ImageY16    (flipVertically x)
    ImageYF     x -> ImageYF     (flipVertically x)
    ImageYA8    x -> ImageYA8    (flipVertically x)
    ImageYA16   x -> ImageYA16   (flipVertically x)
    ImageRGB8   x -> ImageRGB8   (flipVertically x)
    ImageRGB16  x -> ImageRGB16  (flipVertically x)
    ImageRGBF   x -> ImageRGBF   (flipVertically x)
    ImageRGBA8  x -> ImageRGBA8  (flipVertically x)
    ImageRGBA16 x -> ImageRGBA16 (flipVertically x)
    ImageYCbCr8 x -> ImageYCbCr8 (flipVertically x)
    ImageCMYK8  x -> ImageCMYK8  (flipVertically x)
    ImageCMYK16 x -> ImageCMYK16 (flipVertically x)

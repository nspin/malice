module Mal.Extra.Binascii
    ( asHex
    , asNice
    , asOk

    , showHex
    , showNice
    , showOk

    , unHex
    , readHex
    ) where

import Data.Array
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import qualified Data.ByteString as B


asHex :: Word8 -> String
asHex b = [digits ! shiftR b 4, digits ! (b .&. 0x0f)]

digits :: Array Word8 Char
digits = listArray (0, 15) $ ['0'..'9'] ++ ['a'..'f']

unHex :: (Char, Char) -> Word8
unHex (x, y) = shiftL (fromIntegral a) 4 .|. (fromIntegral b)
  where
    Just a = elemIndex x $ elems digits
    Just b = elemIndex y $ elems digits

showHex :: B.ByteString -> String
showHex = intercalate " " . map asHex . B.unpack

readHex :: String -> B.ByteString
readHex = B.pack . map unHex . pairs . filter (not . isSpace)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:zs) = (x, y) : pairs zs


maskWith :: (Char -> Bool) -> Word8 -> Char
maskWith p b = let c = chr (fromIntegral b) in if p c then c else '.'

asNice :: Word8 -> Char
asNice = maskWith isPrint

asOk :: Word8 -> Char
asOk = maskWith $ \c -> isPrint c || elem c " \t\n\r"

showNice :: B.ByteString -> String
showNice = map asNice . B.unpack

showOk :: B.ByteString -> String
showOk = map asOk . B.unpack

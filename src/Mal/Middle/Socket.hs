module Mal.Middle.Socket
    ( recvWithFlags
    , recvBufWithFlags

    , mSG_DONTWAIT
    , mSG_OOB
    , mSG_PEEK
    , mSG_TRUNC
    , mSG_WAITALL

    , getSockOpt
    , getSockOptBuf

    , sOL_IP
    ) where

import Mal.Middle.Socket.Internal

import Data.Bits
import Data.ByteString.Internal
import Data.Word
import Foreign.C.Types (CInt)
import Network.Socket
import qualified Data.ByteString as B

-- | Like 'Network.Socket.ByteString.recv', but also propegates flags to the C function @recv@.
--
-- > recvWithFlags flags sock nbytes
--
recvWithFlags :: Int -> Socket -> Int -> IO ByteString
recvWithFlags flags sock nbytes = createAndTrim nbytes $ \ptr ->
    recvBufWithFlags flags sock ptr nbytes


getSockOpt :: Socket -> CInt -> CInt -> Int -> IO ByteString
getSockOpt sock level option nbytes = createAndTrim nbytes $ \ptr ->
    getSockOptBuf sock level option ptr nbytes

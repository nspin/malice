#include <sys/socket.h>
#include <linux/socket.h>
#include <linux/netfilter_ipv4.h>

module Mal.Middle.Socket.Internal
    ( recvBufWithFlags

    , mSG_CMSG_CLOEXEC 
    , mSG_DONTWAIT 
    , mSG_ERRQUEUE 
    , mSG_OOB 
    , mSG_PEEK 
    , mSG_TRUNC 
    , mSG_WAITALL 

    , mkInvalidArgError
    , mkEOFError

    , getSockOptBuf
    , sOL_IP
    , sO_ORIGINAL_DST
    ) where

import Data.Word
import Foreign.C.Types (CChar, CInt(..), CSize(..))
import Foreign.Marshal.Utils
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable
import GHC.IO.Exception
import Network.Socket
import Network.Socket.Internal
import System.IO.Error


foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CInt

recvBufWithFlags :: Int -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufWithFlags flags sock@(MkSocket fd _ _ _ _) ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidArgError this)
    | otherwise = throwSocketErrorWaitRead sock this $
        fromIntegral <$> c_recv fd (castPtr ptr) (fromIntegral nbytes) (fromIntegral flags)
    where
        this = "Mal.Middle.Socket.Internal.recvBufWithFlags"


mSG_CMSG_CLOEXEC :: Int
mSG_CMSG_CLOEXEC = (#const MSG_CMSG_CLOEXEC)

mSG_DONTWAIT :: Int
mSG_DONTWAIT = (#const MSG_DONTWAIT)

mSG_ERRQUEUE :: Int
mSG_ERRQUEUE = (#const MSG_ERRQUEUE)

mSG_OOB :: Int
mSG_OOB = (#const MSG_OOB)

mSG_PEEK :: Int
mSG_PEEK = (#const MSG_PEEK)

mSG_TRUNC :: Int
mSG_TRUNC = (#const MSG_TRUNC)

mSG_WAITALL :: Int
mSG_WAITALL = (#const MSG_WAITALL)


foreign import ccall unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr Word8 -> Ptr CInt -> IO CInt

getSockOptBuf :: Socket -> CInt -> CInt -> Ptr Word8 -> Int -> IO Int
getSockOptBuf sock@(MkSocket fd _ _ _ _) level option ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidArgError this)
    | otherwise = with (fromIntegral nbytes) $ \n -> do
        throwSocketErrorIfMinus1Retry_ this $
            c_getsockopt fd level option ptr n
        fromIntegral <$> peek n
    where
        this = "Network.Socket.Extra.Internal.getSockOptBuf"


sOL_IP :: CInt
sOL_IP = 0
-- SOL_IP is in linux/
-- sOL_IP = (#const SOL_IP)

sO_ORIGINAL_DST :: CInt
sO_ORIGINAL_DST = (#const SO_ORIGINAL_DST)


mkInvalidArgError :: String -> IOError
mkInvalidArgError loc = ioeSetErrorString
    (mkIOError InvalidArgument loc Nothing Nothing)
    "non-positive length"

mkEOFError :: String -> IOError
mkEOFError loc = ioeSetErrorString
    (mkIOError EOF loc Nothing Nothing)
    "end of file"

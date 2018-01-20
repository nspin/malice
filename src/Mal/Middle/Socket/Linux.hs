module Mal.Middle.Socket.Linux
    ( mSG_CMSG_CLOEXEC 
    , mSG_ERRQUEUE 
    , sO_ORIGINAL_DST

    , getOriginalDst
    ) where

import Mal.Middle.Socket.Internal.Linux

getOriginalDst :: Socket -> IO (PortNumber, HostAddress)
getOriginalDst sock = ((,) <$> port <*> host) <$> getSockOpt sock sOL_IP sO_ORIGINAL_DST 16
  where
    port bs = fromIntegral (shiftL (fromIntegral (B.index bs 2)) 8 .|. fromIntegral (B.index bs 3) :: Word16)
    host bs = tupleToHostAddress (B.index bs 4, B.index bs 5, B.index bs 6, B.index bs 7)

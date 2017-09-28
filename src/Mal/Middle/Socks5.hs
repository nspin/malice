{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mal.Middle.Socks5
    ( socks5Handler
    , socksAddrToHostName
    ) where

import Mal.Monad
import Mal.Middle.TCP

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Monoid
import Data.Text (pack)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.Socks5.Lowlevel
import Network.Socks5.Types


-- | Given a raw connection to a client, obtain a connection to the client's
-- target server and yield control to a man in the middle.
socks5Handler :: (MonadIO m, MonadCatch m, MonadMask m, MonadLogger m) => (TCPProxyCtx -> m ()) -> Connection -> m ()
socks5Handler mitm (Connection client addr) = do
    r <- evalVertexT' link $ Vertex
            { edgeIn = liftIO $ recv client 4096
            , edgeOut = liftIO . sendAll client
            }
    case r of
        Left err -> $logInfo $ "socks5 link error from " <> pack (show addr) <> ": " <> pack err
        Right (saddr, server) -> do
            let handle ex = $logWarn $
                    "mitm from " <> pack (show addr) <> " threw exception: " <> pack (show (ex :: SomeException))
                cleanup = liftIO $ close server
            mitm (TCPProxyCtx client server) `catch` handle `finally` cleanup


link :: (MonadIO m, MonadCatch m, MonadVertex String m) => m (SocksAddress, Socket)
link = do
    SocksHello meths <- awaitGet
    when (not (SocksMethodNone `elem` meths)) $ do
        yieldPut $ SocksHelloResponse SocksMethodNotAcceptable
        raise "null authentication not in client methods"
    yieldPut $ SocksHelloResponse SocksMethodNone
    SocksRequest cmd socksAddr port <- awaitGet
    when (cmd /= SocksCommandConnect) $ do
        yieldPut $ SocksResponse
            (SocksReplyError SocksErrorCommandNotSupported)
            (SocksAddrDomainName "go.away")
            0xdead
        raise "method is not connect"
    let saddr = SocksAddress socksAddr port
    (family, addr) <- liftIO (resolveSocksAddr saddr) >>= either raise return
    sock <- liftIO $ socket family Stream defaultProtocol
    liftIO (connect sock addr) `catch` \(ex :: SomeException) -> do
        yieldPut $ SocksResponse
            (SocksReplyError SocksErrorHostUnreachable)
            (SocksAddrDomainName "go.away")
            0xdead
        raise $ show ex
    SocksAddress host port <- liftIO (resolveSockAddr <$> getSocketName sock) >>= either raise return
    yieldPut $ SocksResponse (SocksReplySuccess) host port
    return (saddr, sock)


resolveSockAddr :: SockAddr -> Either String SocksAddress
resolveSockAddr (SockAddrInet port h) = Right $ SocksAddress (SocksAddrIPV4 h) port
resolveSockAddr (SockAddrInet6 port fid h6 sid) = Left "IPv6 not supported"
resolveSockAddr _ = Left "wft"


resolveSocksAddr :: SocksAddress -> IO (Either String (Family, SockAddr))
resolveSocksAddr (SocksAddress (SocksAddrIPV4 h) port) = return . Right $ (AF_INET, SockAddrInet port h)
resolveSocksAddr (SocksAddress (SocksAddrIPV6 h6) port) = return $ Left "IPv6 not supported"
resolveSocksAddr (SocksAddress (SocksAddrDomainName dn) port) = do
    infos <- getAddrInfo Nothing (Just (C.unpack dn)) Nothing
    return $ case infos of
        [] -> Left $ "could not resolve domain name " ++ C.unpack dn
        info:_ -> Right $ (addrFamily info, addrAddress info)


socksAddrToHostName :: SocksAddress -> Network.Socket.HostName
socksAddrToHostName (SocksAddress (SocksAddrIPV4 h) _) = let (a, b, c, d) = hostAddressToTuple h in intercalate "." $ map show [a, b, c, d]
socksAddrToHostName (SocksAddress (SocksAddrDomainName dn) _) = C.unpack dn

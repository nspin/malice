{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections #-}

module Mal.Protocol.TLS
    ( malTLS
    , KeySwapper
    , makeKeySwapper
    , Contexts(..)
    , fromContexts
    ) where

import Mal.Monad

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Unlift
import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Default.Class
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Data.X509
import Data.X509.File
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.TLS hiding (SHA256, HashSHA256)
import Network.TLS.Extra


type KeySwapper = CertificateChain -> Credential

data Contexts = Contexts
    { clientCtx :: Context
    , serverCtx :: Context
    }

malTLS :: (MonadIO m, MonadLogger m, MonadBaseUnlift IO m)
       => KeySwapper
       -> Backend
       -> Backend
       -> (Contexts -> m ())
       -> m ()
malTLS swapKey client server mitm = do

    UnliftBase run <- askUnliftBase

    (clientCtx, serverCtx) <- liftIO $ do

            chainSpot <- newEmptyMVar
            hostSpot <- newEmptyMVar

            let doTrade mhost = run $ do
                    $logDebug $ "host: " <> pack (show mhost)
                    chain <- liftIO $ do
                        putMVar hostSpot mhost
                        takeMVar chainSpot -- TODO(nickspinale) will this ever remain empty?
                    let swapped = swapKey chain
                    $logDebug $ "swap: " <> pack (show chain) <> " -> " <> pack (show swapped)
                    return $ Credentials [swapped]

                handshakeHook label handshake = handshake <$ (run . $logDebug $ "handshake recv: " <> pack label <> ": " <> pack (show handshake))

            clientCtx <- contextNew client $ (def :: ServerParams)
                        { serverSupported = def
                            { supportedCiphers = ciphersuite_default
                            }
                        , serverHooks = def
                            { onServerNameIndication = doTrade
                            }
                        }
            contextHookSetHandshakeRecv clientCtx $ handshakeHook "CLIENT"
            clientHandshake <- async $ handshake clientCtx

            mhost <- takeMVar hostSpot

            let onCert store cache sid chain = [] <$ putMVar chainSpot chain
                f params = params
                      { clientHooks = def
                          { onServerCertificate = onCert
                          }
                      , clientSupported = def
                          { supportedCiphers = ciphersuite_all
                          }
                      }
                serverParams = f $ case mhost of
                    Just host -> defaultParamsClient host B.empty
                    Nothing -> (defaultParamsClient "" B.empty)
                        { clientUseServerNameIndication = False
                        }

            serverCtx <- contextNew server serverParams
            contextHookSetHandshakeRecv serverCtx $ handshakeHook "SERVER"
            handshake serverCtx
            wait clientHandshake
            return (clientCtx, serverCtx)

    mitm $ Contexts clientCtx serverCtx
    liftIO $ do
        bye clientCtx
        bye serverCtx


makeKeySwapper :: Certificate -> PrivateKey -> PrivateKey -> KeySwapper
makeKeySwapper rootCert rootPriv myPriv (CertificateChain (signedOldCert:_)) = (CertificateChain [signedNewCert], PrivKeyRSA myPriv)
  where
    oldCert = signedObject $ getSigned signedOldCert
    Right signedNewCert = objectToSignedExactF f newCert
    f bs = (, sigAlg) <$> sign Nothing (Just SHA256) rootPriv bs
    sigAlg = SignatureALG HashSHA256 PubKeyALG_RSA
    newCert = oldCert
        { certVersion = 3
        , certSerial = certSerial oldCert + 1
        , certSignatureAlg = sigAlg
        , certIssuerDN = certSubjectDN rootCert
        , certPubKey = PubKeyRSA $ private_pub myPriv
        , certExtensions = Extensions $ Just newExts
        }
    Extensions oldExts = certExtensions oldCert
    newExts = filter (not <$> ((||) <$> isAKID <*> isSKID)) $ fromMaybe [] oldExts
    -- newExts = [ extensionEncode False $ ExtKeyUsage [ KeyUsage_keyCertSign ]
    --           , extensionEncode False $ ExtSubjectKeyId kh
    --           , extensionEncode False $ ExtAuthorityKeyId kh
    --           ]


isAKID :: ExtensionRaw -> Bool
isAKID raw = case extensionDecode raw of
    Nothing -> False
    Just (Right (_ :: ExtAuthorityKeyId)) -> True

isSKID :: ExtensionRaw -> Bool
isSKID raw = case extensionDecode raw of
    Nothing -> False
    Just (Right (_ :: ExtSubjectKeyId)) -> True


fromContexts :: MonadIO m => Contexts -> Vertices m
fromContexts (Contexts client server) = Vertices
    (Vertex (recvData client) (sendData client . L.fromStrict))
    (Vertex (recvData server) (sendData server . L.fromStrict))

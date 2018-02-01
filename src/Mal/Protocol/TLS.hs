{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Crypto.Hash hiding (Context)
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.Bits
import qualified Data.ByteArray as A
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


fromContexts :: MonadIO m => Contexts -> Vertices m
fromContexts (Contexts client server) = Vertices
    (Vertex (recvData client) (sendData client . L.fromStrict))
    (Vertex (recvData server) (sendData server . L.fromStrict))


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

                handshakeHook label handshake = handshake <$
                    (run . $logDebug $ "handshake recv: " <> pack label <> ": " <> pack (show handshake))

            clientCtx <- contextNew client $ (def :: ServerParams)
                        { serverSupported = def
                            { supportedCiphers = ciphersuite_default
                            }
                        , serverHooks = def
                            { onServerNameIndication = doTrade
                            }
                        }
            contextHookSetHandshakeRecv clientCtx $ handshakeHook "client"
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
            contextHookSetHandshakeRecv serverCtx $ handshakeHook "server"
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
        -- , certSerial = certSerial oldCert + 1
        , certSerial = uniqueSerialNumber myPriv signedOldCert
        , certSignatureAlg = sigAlg
        , certIssuerDN = certSubjectDN rootCert
        , certPubKey = PubKeyRSA $ private_pub myPriv
        , certExtensions = stripExts (certExtensions oldCert)
        }

uniqueSerialNumber :: Crypto.PubKey.RSA.PrivateKey -> SignedExact Certificate -> Integer
uniqueSerialNumber priv signed = foldl f 0 . take 10 . A.unpack . hashWith SHA256 $
    encodeASN1' DER (toASN1 priv []) <> getSignedData signed
  where
    f acc b = shiftL acc 8 .|. (toInteger b)

stripExts :: Extensions -> Extensions
stripExts (Extensions mexts) = Extensions $ do
    exts <- mexts
    case filter p exts of
        [] -> Nothing
        exts' -> Just exts'
  where
    p ext = not $ extRawOID ext `elem`
        [ extOID (undefined :: ExtAuthorityKeyId)
        , extOID (undefined :: ExtSubjectKeyId)
        , extOID (undefined :: ExtCrlDistributionPoints)
        , extOID (undefined :: ExtAuthorityInformationAccess)
        ]


-- TODO(nspin) complete and push upstream

data ExtAuthorityInformationAccess = ExtAuthorityInformationAccess -- TODO
    deriving (Show, Eq)

instance Extension ExtAuthorityInformationAccess where
    extOID _ = [1, 3, 6, 1, 5, 5, 7, 1, 1]
    extHasNestedASN1 = const True
    extEncode = error "extEncode ExtAuthorityInformationAccess unimplemented"
    extDecode = error "extDecode ExtAuthorityInformationAccess unimplemented"


-- not sure why this didn't make it into cryptonite:
--  https://hackage.haskell.org/package/crypto-pubkey-types-0.4.3/docs/Crypto-Types-PubKey-RSA.html
--  https://hackage.haskell.org/package/cryptonite-0.24/docs/Crypto-PubKey-RSA-Types.html
instance ASN1Object PrivateKey where
    toASN1 privKey = \xs -> Start Sequence
                          : IntVal 0
                          : IntVal (public_n $ private_pub privKey)
                          : IntVal (public_e $ private_pub privKey)
                          : IntVal (private_d privKey)
                          : IntVal (private_p privKey)
                          : IntVal (private_q privKey)
                          : IntVal (private_dP privKey)
                          : IntVal (private_dQ privKey)
                          : IntVal (fromIntegral $ private_qinv privKey)
                          : End Sequence
                          : xs
    fromASN1 (Start Sequence
             : IntVal 0
             : IntVal n
             : IntVal e
             : IntVal d
             : IntVal p1
             : IntVal p2
             : IntVal pexp1
             : IntVal pexp2
             : IntVal pcoef
             : End Sequence
             : xs) = Right (privKey, xs)
        where calculate_modulus n i = if (2 ^ (i * 8)) > n then i else calculate_modulus n (i+1)
              privKey = PrivateKey
                        { private_pub  = PublicKey { public_size = calculate_modulus n 1
                                                   , public_n    = n
                                                   , public_e    = e
                                                   }
                        , private_d    = d
                        , private_p    = p1
                        , private_q    = p2
                        , private_dP   = pexp1
                        , private_dQ   = pexp2
                        , private_qinv = pcoef
                        }

    fromASN1 ( Start Sequence
             : IntVal 0
             : Start Sequence
             : OID [1, 2, 840, 113549, 1, 1, 1]
             : Null
             : End Sequence
             : OctetString bs
             : xs
             ) = let inner = either strError fromASN1 $ decodeASN1' BER bs
                     strError = Left .
                                ("fromASN1: RSA.PrivateKey: " ++) . show
                 in either Left (\(k, _) -> Right (k, xs)) inner
    fromASN1 _ =
        Left "fromASN1: RSA.PrivateKey: unexpected format"

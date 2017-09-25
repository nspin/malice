{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Crypto.Random
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import qualified Data.ByteString as B
import Data.Monoid
import Data.PEM
import Data.X509
import Options.Applicative
import Time.Types


data Opts = Opts
    { seed :: Integer
    , certPath :: String
    , privPath :: String
    }

opts :: Parser Opts
opts = Opts
    <$> option auto
        (  long "seed"
        <> short 's'
        <> metavar "SEED"
        <> value 0
        <> showDefault
        <> help "Integer seed for key generation"
        )  
    <*> argument str (metavar "CERT_OUT" <> help "Output path for certificate")
    <*> argument str (metavar "PRIV_OUT" <> help "Output path for key")

parser = info (opts <**> helper)
    (  fullDesc
    <> progDesc "Generate key and self-signed certificate for testing"
    )

main :: IO ()
main = do
    Opts{..} <- execParser parser
    let rootPriv = privateKeyWithSeed seed
        rootCert = makeRootCert rootPriv
    B.writeFile privPath . pemWriteBS . PEM "RSA PRIVATE KEY" [] $ encodeRSA rootPriv
    B.writeFile certPath . pemWriteBS . PEM "CERTIFICATE" [] $ encodeSignedObject rootCert


keyHash :: PublicKey -> B.ByteString
keyHash pub = encodeASN1' DER $ toASN1 (PubKeyRSA pub) []


getSubjectKeyId :: Certificate -> Maybe B.ByteString
getSubjectKeyId cert = case extensionGet (certExtensions cert) of
    Just (ExtSubjectKeyId bs) -> Just bs
    Nothing -> Nothing


privateKeyWithSeed :: Integer -> PrivateKey
privateKeyWithSeed i = priv
  where
    ((pub, priv), g) = withDRG (drgNewSeed (seedFromInteger i)) (generate 256 3)


encodeRSA :: PrivateKey -> B.ByteString
encodeRSA = encodeASN1' DER . rsaToASN1

rsaToASN1 :: PrivateKey -> [ASN1]
rsaToASN1 priv = 
    [ Start Sequence
    , IntVal 0
    , IntVal public_n
    , IntVal public_e
    , IntVal private_d
    , IntVal private_p
    , IntVal private_q
    , IntVal private_dP
    , IntVal private_dQ
    , IntVal private_qinv
    , End Sequence
    ]
  where
    PrivateKey{..} = priv
    PublicKey{..} = private_pub


makeRootCert :: PrivateKey -> SignedCertificate
makeRootCert priv = signed
  where
    Right signed = objectToSignedExactF f cert
    f bs = (, sigAlg) <$> sign Nothing (Just SHA256) priv bs
    sigAlg = SignatureALG HashSHA256 PubKeyALG_RSA
    cert = Certificate
        { certVersion = 3
        , certSerial = 0x1337
        , certSignatureAlg = sigAlg
        , certIssuerDN = dn
        , certValidity = (start, end)
        , certSubjectDN = dn
        , certPubKey = PubKeyRSA $ private_pub priv
        , certExtensions = Extensions $ Just exts
        }
    dn = DistinguishedName [(getObjectID DnCommonName, ASN1CharacterString UTF8 "me")]
    start = DateTime (Date 1970 January 1) (TimeOfDay 0 0 0 0)
    end = DateTime (Date 2070 January 1) (TimeOfDay 0 0 0 0)
    exts = [ extensionEncode False $ ExtBasicConstraints True Nothing
           , extensionEncode False $ ExtKeyUsage [ KeyUsage_keyCertSign ]
           , extensionEncode False $ ExtSubjectKeyId kh
           , extensionEncode False $ ExtAuthorityKeyId kh
           ]
    kh = keyHash $ private_pub priv
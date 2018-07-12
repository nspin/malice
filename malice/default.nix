{ mkDerivation, array, asn1-encoding, asn1-types, async, attoparsec
, base, base16-bytestring, bytestring, case-insensitive, cereal
, cryptonite, data-default-class, exceptions, hourglass, http-types
, JuicyPixels, JuicyPixels-extra, lens, malice-core
, malice-core-attoparsec, malice-core-serialize, memory
, monad-logger, monad-unlift, mtl, network, optparse-applicative
, pcap, pem, socks, stdenv, text, tls, transformers, x509
, x509-store, x509-validation
}:
mkDerivation {
  pname = "malice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array asn1-encoding asn1-types async attoparsec base
    base16-bytestring bytestring case-insensitive cereal cryptonite
    data-default-class exceptions http-types lens malice-core
    malice-core-attoparsec malice-core-serialize memory monad-logger
    monad-unlift mtl network pcap socks text tls transformers x509
    x509-store x509-validation
  ];
  executableHaskellDepends = [
    asn1-encoding asn1-types async attoparsec base base16-bytestring
    bytestring case-insensitive cryptonite exceptions hourglass
    http-types JuicyPixels JuicyPixels-extra lens malice-core memory
    monad-logger monad-unlift mtl network optparse-applicative pem
    socks text tls x509 x509-store
  ];
  license = stdenv.lib.licenses.mit;
}

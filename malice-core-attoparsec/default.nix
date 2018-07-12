{ mkDerivation, attoparsec, base, bytestring, malice-core, stdenv
, transformers
}:
mkDerivation {
  pname = "malice-core-attoparsec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring malice-core transformers
  ];
  license = stdenv.lib.licenses.mit;
}

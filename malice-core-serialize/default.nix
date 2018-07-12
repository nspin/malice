{ mkDerivation, base, bytestring, cereal, malice-core, stdenv
, transformers
}:
mkDerivation {
  pname = "malice-core-serialize";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal malice-core transformers
  ];
  license = stdenv.lib.licenses.mit;
}

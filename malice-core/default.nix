{ mkDerivation, base, bytestring, exceptions, lens, mtl, stdenv
, transformers
}:
mkDerivation {
  pname = "malice-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions lens mtl transformers
  ];
  license = stdenv.lib.licenses.mit;
}

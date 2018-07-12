{ mkDerivation, base, bytestring, exceptions, lens, monad-logger
, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "malice-core";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions lens monad-logger mtl transformers
  ];
  license = stdenv.lib.licenses.mit;
}

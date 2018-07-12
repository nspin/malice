{ mkDerivation, base, malice-core, monad-logger, stdenv
, transformers
}:
mkDerivation {
  pname = "malice-core-instances-logger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base malice-core monad-logger transformers
  ];
  license = stdenv.lib.licenses.mit;
}

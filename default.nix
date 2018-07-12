{ haskellPackages }:

rec {
  malice-core = haskellPackages.callPackage ./malice-core {};
  malice-core-serialize = haskellPackages.callPackage ./malice-core-serialize {
    inherit malice-core;
  };
  malice-core-attoparsec = haskellPackages.callPackage ./malice-core-attoparsec {
    inherit malice-core;
  };
  malice = haskellPackages.callPackage ./malice {
    inherit malice-core malice-core-serialize malice-core-attoparsec;
  };
}

#!/bin/sh

pkgs='malice malice-core malice-core-serialize malice-core-attoparsec malice-core-instances-logger'

for pkg in $pkgs; do
    pushd "$(dirname "$0")/../$pkg"
        cabal2nix . > default.nix
    popd
done

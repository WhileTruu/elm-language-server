{
  pkgs,
  lib,
  makeWrapper,
}: let
  # Haskell packages that require ghc 9.14
  hs914Pkgs = import ./packages {inherit pkgs lib makeWrapper;};

  assembleScope = self: basics:
    (hs914Pkgs self).elmPkgs // basics;
in
  lib.makeScope pkgs.newScope
  (
    self: assembleScope self {}
  )

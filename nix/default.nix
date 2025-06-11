{
  pkgs,
  lib,
  makeWrapper,
}: let
  # Haskell packages that require ghc 9.6
  hs96Pkgs = import ./packages {inherit pkgs lib makeWrapper;};

  assembleScope = self: basics:
    (hs96Pkgs self).elmPkgs // basics;
in
  lib.makeScope pkgs.newScope
  (
    self: assembleScope self {}
  )

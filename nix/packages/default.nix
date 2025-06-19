{
  pkgs,
  lib,
  makeWrapper,
}: self:
pkgs.haskell.packages.ghc96.override {
  overrides = self: super: let
    inherit (pkgs.haskell.lib.compose) overrideCabal;
    elmPkgs = rec {
      elm-language-server = overrideCabal (drv: {
        # sadly parallelism most of the time breaks compilation
        enableParallelBuilding = false;
        patches = [];
        buildTools = drv.buildTools or [] ++ [makeWrapper];

        description = "Fork of the elm compiler, repurposed for a language server.";
        homepage = "https://github.com/WhileTruu/elm-language-server";
        license = lib.licenses.bsd3;
        maintainers = with lib.maintainers; [];
        mainProgram = "elm-language-server";
      }) (self.callPackage ./elm-language-server {});

      # Fix TLS issues
      # see https://github.com/elm/compiler/pull/2325
      # and https://discourse.elm-lang.org/t/elm-init-tls-issue/10231/11
      tls = self.callPackage ./tls-1.9.0.nix { };
    };
  in
    elmPkgs
    // {
      inherit elmPkgs;

      ansi-wl-pprint = overrideCabal (drv: {
        jailbreak = true;
      }) (self.callPackage ./ansi-wl-pprint {});
    };
}

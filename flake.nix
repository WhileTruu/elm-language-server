{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    utils,
  }:
    utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.elm-language-server = (pkgs.callPackage (import ./nix) {}).elm-language-server;
        defaultPackage = packages.elm-language-server;
      }
    );
}

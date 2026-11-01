#!/usr/bin/env nix
#!nix shell nixpkgs#cabal2nix -c bash
cd packages/elm-language-server
cabal2nix ../../.. >default.nix

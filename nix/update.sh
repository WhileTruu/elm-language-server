#!/usr/bin/env nix-shell
#!nix-shell -p cabal2nix -i bash
cd packages/elm-language-server
cabal2nix ../../.. >default.nix

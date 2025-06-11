#!/usr/bin/env nix-shell
#!nix-shell -p cabal2nix elm2nix -i bash
cd packages/elm-language-server
cabal2nix ../../.. --revision 69eed5f91108b74f2bea675917a72c96d01531a7 >default.nix

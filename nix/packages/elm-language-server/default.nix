{ mkDerivation, aeson, ansi-terminal, ansi-wl-pprint, base, binary
, bytestring, containers, directory, edit-distance, file-embed
, filelock, filepath, ghc-prim, haskeline, HTTP, http-client
, http-client-tls, http-types, language-glsl, lib, mtl, network
, parsec, process, raw-strings-qq, scientific, SHA, snap-core
, snap-server, template-haskell, time, tls, unordered-containers
, utf8-string, vector, zip-archive
}:
mkDerivation {
  pname = "elm-language-server";
  version = "0.3";
  src = ../../..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal ansi-wl-pprint base binary bytestring
    containers directory edit-distance file-embed filelock filepath
    ghc-prim haskeline HTTP http-client http-client-tls http-types
    language-glsl mtl network parsec process raw-strings-qq scientific
    SHA snap-core snap-server template-haskell time tls
    unordered-containers utf8-string vector zip-archive
  ];
  homepage = "https://github.com/WhileTruu/elm-language-server";
  description = "Fork of the Elm compiler, repurposed for a language server";
  license = lib.licenses.bsd3;
  mainProgram = "elm-language-server";
}

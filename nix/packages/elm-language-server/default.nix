{ mkDerivation, aeson, ansi-terminal, ansi-wl-pprint, base, binary
, bytestring, containers, directory, edit-distance, filelock
, filepath, ghc-prim, haskeline, HTTP, http-client, http-client-tls
, http-types, language-glsl, lib, mtl, network, parsec, process
, scientific, SHA, snap-core, snap-server, template-haskell, time
, unordered-containers, utf8-string, vector, zip-archive
}:
mkDerivation {
  pname = "elm-language-server";
  version = "1.3.0";
  src = ../../..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal ansi-wl-pprint base binary bytestring
    containers directory edit-distance filelock filepath ghc-prim
    haskeline HTTP http-client http-client-tls http-types language-glsl
    mtl network parsec process scientific SHA snap-core snap-server
    template-haskell time unordered-containers utf8-string vector
    zip-archive
  ];
  homepage = "https://github.com/WhileTruu/elm-language-server";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "elm-language-server";
}

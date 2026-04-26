# WhileTruu's Elm language server

An `elm-compiler` based language server implementation for the Elm programming language.

# Why?

A language server should be *fast* and *reliable*. The Elm compiler is both of those things, a language server built from it might be as well.

## Features ✨

- 🧭 Go to definition
- 🔍 Find references (supports top level values, infix operators, aliases, custom types (but not variants))
- 🛡️ Diagnostics (errors and warnings)
- 🏷️ Document symbols
- 🧹 Formatting with `elm-format` (enabled when executable is available on init)
- ✏️ Rename (based on references, supports the same stuff)

## Configuration
- `initializationOptions.whiletruu-elm-language-server.elmFormatPath` formatter executable, defaults to `elm-format`

## Try it out quickly via Nix

If you have Nix installed and flakes enabled (which is the default when
installing with the [Determinate Systems Installer][ds-nix]), you can easily
compile and run this project without installing it or its build tools.

First, run the following command to make sure your system is able to fetch and
build everything. This could take a few minutes the first time you run it:

    nix run github:WhileTruu/elm-language-server -- --help

You should see a bunch of activity as Nix downloads and builds things, and then
a short message from the language server, like this:

    Start the Elm language server:

        elm-language-server

    The server listens on stdin.

Great! It works. Now, pick one of the following methods to make your
IDE/editor's LSP integration use the language server:

1. Configure your IDE to use this command for the Elm language server:

       nix run github:WhileTruu/elm-language-server

2. For an editor that inherits environment from your command line, you can also
   start a sub-shell with `elm-language-server` available in `PATH`, and then
   start your editor from within that shell. For instance, by running:

       nix shell github.com:WhileTruu/elm-language-server
       nvim ./src/Main.elm

   You may need to alter your settings to use `elm-language-server` as the
   elmls command in your LSP configuration.

[ds-nix]: https://github.com/DeterminateSystems/nix-installer?tab=readme-ov-file#determinate-nix-installer

## Install

### Prerequisites

- GHC version `9.2.8`
- Cabal version `3.10.3.0`

### Build
`cabal new-build --ghc-option=-split-sections` seems to work!

### Development

haskell-language-server version 2.9.0.0 works.
install with `ghcup install hls 2.9.0.0`

# Acknowledgements

These projects were of a lot of help:

* [elm-tooling/elm-language-server](https://github.com/elm-tooling/elm-language-server)
* [mdgriffith/elm-dev](https://github.com/mdgriffith/elm-dev)


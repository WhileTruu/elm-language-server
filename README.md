# 🚧 WhileTruu's Elm language server 🚧

An **experimental** language server implementation for the Elm programming language.

# Why?

A language server should be *fast* and *reliable*. The Elm compiler is both of those things, a language server built from it might be as well.

## Features ✨

- 🧭 Go to definition
- 🔍 Find references
- 🛡️ Diagnostics (errors and warnings)

## Install

### Prerequisites

- GHC version `9.2.8`
- Cabal version `3.10.3.0`

### Build 
`cabal new-build --ghc-option=-split-sections` seems to work! 

# Acknowledgements

These projects were of a lot of help:

* [elm-tooling/elm-language-server](https://github.com/elm-tooling/elm-language-server)
* [mdgriffith/elm-dev](https://github.com/mdgriffith/elm-dev)

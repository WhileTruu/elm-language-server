# WhileTruu's Elm language server.

An experimental language server implementation for the Elm programming language.

## Features

- Go to definition
- Find references
- Diagnostics (errors and warnings)

## Install

### Prerequisites

- GHC version `9.2.8`
- Cabal version `3.10.3.0`

### Build 
`cabal new-build --ghc-option=-split-sections` seems to work! 

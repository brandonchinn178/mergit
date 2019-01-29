#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

# Dependencies that take a lot of memory that shouldn't run in parallel
stack build Cabal

stack build --test --only-dependencies
stack build hlint stylish-haskell

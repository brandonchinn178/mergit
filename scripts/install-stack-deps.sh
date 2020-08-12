#!/bin/bash
#
# Install third-party stack dependencies.

set -ex -o pipefail

# Dependencies that take a lot of memory that shouldn't run in parallel
stack build haskell-src-meta

stack build --test --only-dependencies
stack build hlint stylish-haskell

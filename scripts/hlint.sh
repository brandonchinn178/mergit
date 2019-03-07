#!/bin/bash
#
# Runs HLint and errors if any hints are found.

set -eo pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

git ls-files '*.hs' -z | xargs -0 stack exec hlint

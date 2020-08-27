#!/usr/bin/env bash

set -eux -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

if [[ ! -d .graphql/schema.graphql ]]; then
    curl -sSL -o .graphql/schema.graphql \
        https://docs.github.com/public/schema.docs.graphql
fi

stack exec graphql-codegen

scripts/stylish-haskell.sh --apply \
    merge-bot-core/src/MergeBot/Core/GraphQL/API.hs \
    merge-bot-core/src/MergeBot/Core/GraphQL/Enums/*.hs

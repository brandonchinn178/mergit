#!/usr/bin/env bash
#
# Generate Bazel definitions for Stackage dependencies using Hazel.
# https://github.com/formationai/hazel

set -o nounset -o pipefail -x

TOP="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

STACK_LTS=$(sed -n 's/^resolver: \(.*\)/\1/p' "${TOP}/stack.yaml")

WORKDIR=$(mktemp -d)
trap "rm -rf '${WORKDIR}'" 0

git clone https://github.com/formationai/hazel "${WORKDIR}"
# reuse packages from the current stack lts
sed -i.bak "s/lts-10.5/${STACK_LTS}/" "${WORKDIR}/Stackage.hs"
(cd "${WORKDIR}" && "${WORKDIR}/Stackage.hs" "${STACK_LTS}" "${TOP}/bazel/packages.bzl")

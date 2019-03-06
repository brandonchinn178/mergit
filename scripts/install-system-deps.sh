#!/bin/bash
#
# Install system dependencies.

set -o nounset -o pipefail -x

function is_command() {
    type "$1" &> /dev/null
}

function install_bazel() {
    local PLATFORM=$1
    local BAZEL_VERSION=0.23.1
    local BASE_URL='https://github.com/bazelbuild/bazel/releases/download'
    local INSTALL_SCRIPT_NAME="bazel-${BAZEL_VERSION}-installer-${PLATFORM}-x86_64.sh"

    local TMP_DIR=$(mktemp -d)
    local INSTALL_SCRIPT="${TMP_DIR}/${INSTALL_SCRIPT_NAME}"

    curl -L "${BASE_URL}/${BAZEL_VERSION}/${INSTALL_SCRIPT_NAME}" -o "${INSTALL_SCRIPT}"
    chmod +x "${INSTALL_SCRIPT}" && "${INSTALL_SCRIPT}"
    rm -rf "${TMP_DIR}"
}

function install_linux() {
    if is_command yum; then
        yum update -y --exclude=filesystem
        yum install -y zlib-devel || exit
    fi
    install_bazel 'linux'
}

function install_darwin() {
    install_bazel 'darwin'
}

function install_darwin_dev() {
    if is_command npm; then
        if ! is_command smee; then
            npm install -g smee-client || exit
        fi
    else
        echo "NPM is not installed" >&2
        exit 1
    fi
}

case "$(uname)" in
    (Linux)
        install_linux
    ;;
    (Darwin)
        if [[ -z "${CI:-}" ]]; then
            install_darwin_dev
        fi
        install_darwin
    ;;
esac

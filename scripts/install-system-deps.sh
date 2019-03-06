#!/bin/bash
#
# Install system dependencies.

set -o nounset -o pipefail -x

function is_command() {
    type "$1" &> /dev/null
}

# installs stack to /usr/local/bin/stack
function install_stack() {
    if is_command stack; then
        return
    fi

    local SUFFIX=$1
    local STACK_VERSION=1.9.3
    local BASE_URL=https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/
    local STACK_ARCHIVE=stack-${STACK_VERSION}-${SUFFIX}.tar.gz

    local TMP_DIR=$(mktemp -d)
    curl -L "${BASE_URL}/${STACK_ARCHIVE}" -o "${TMP_DIR}/${STACK_ARCHIVE}" || exit
    tar xzf "${TMP_DIR}/${STACK_ARCHIVE}" -C /usr/local/bin/ --strip-components=1 || exit
    rm -rf "${TMP_DIR}"
}

# installs bazel to /usr/local/bin/bazel
function install_bazel() {
    if is_command bazel; then
        return
    fi

    local PLATFORM=$1
    local BAZEL_VERSION=0.23.1
    local BASE_URL='https://github.com/bazelbuild/bazel/releases/download'
    local INSTALL_SCRIPT_NAME="bazel-${BAZEL_VERSION}-installer-${PLATFORM}-x86_64.sh"

    local TMP_DIR=$(mktemp -d)
    local INSTALL_SCRIPT="${TMP_DIR}/${INSTALL_SCRIPT_NAME}"

    curl -L "${BASE_URL}/${BAZEL_VERSION}/${INSTALL_SCRIPT_NAME}" -o "${INSTALL_SCRIPT}" || exit
    chmod +x "${INSTALL_SCRIPT}" && "${INSTALL_SCRIPT}" || exit
    rm -rf "${TMP_DIR}"
}

function install_linux() {
    if is_command yum; then
        yum update -y --exclude=filesystem

        local YUM_DEPS=(
            # stack dependencies
            gcc
            gmp-devel
            # bazel dependencies
            unzip
            which
            # haskell dependencies
            zlib-devel
        )

        yum install -y --setopt=skip_missing_names_on_install=False "${YUM_DEPS[@]}" || exit
    fi

    # hack for stack
    ln -sf /lib64/libgmp.so /lib64/libgmp.so.3

    install_stack 'linux-x86_64-gmp4'
    install_bazel 'linux'
}

function install_darwin() {
    install_stack 'osx-x86_64'
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

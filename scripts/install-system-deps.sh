#!/bin/bash
#
# Install system dependencies.

set -eo pipefail

function is_command() {
    type "$1" &> /dev/null
}

function install_linux() {
    if is_command yum; then
        yum update -y --exclude=filesystem
        yum install -y zlib-devel
    fi
}

function install_darwin() {
    if is_command npm; then
        if ! is_command smee; then
            npm install -g smee-client
        fi
    else
        echo "NPM is not installed" >&2
        exit 1
    fi
}

case "$(uname)" in
    (Linux) install_linux ;;
    (Darwin) install_darwin ;;
esac

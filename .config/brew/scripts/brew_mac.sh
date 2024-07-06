#!/usr/bin/env bash

# Ensure BREW_PREFIX is available
if [ -z "$BREW_PREFIX" ]; then
    echo "BREW_PREFIX is not set. Please run this script through brew.sh."
    exit 1
fi

# Install more recent versions of some macOS tools.
brew install grep  # Improved version of grep
brew install openssh  # OpenSSH for secure shell access
brew install screen  # Terminal multiplexer
brew install gmp  # Library for arbitrary-precision arithmetic

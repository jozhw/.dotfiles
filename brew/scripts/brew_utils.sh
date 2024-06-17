#!/usr/bin/env bash

# Ensure BREW_PREFIX is available
if [ -z "$BREW_PREFIX" ]; then
    echo "BREW_PREFIX is not set. Please run this script through brew.sh."
    exit 1
fi

# Install GNU core utilities (those that come with macOS are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.

brew install coreutils
ln -s "${BREW_PREFIX}/bin/gsha256sum" "${BREW_PREFIX}/bin/sha256sum"

# Install some other useful utilities like `sponge`.
brew install moreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils

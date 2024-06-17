#!/usr/bin/env bash

# Ensure BREW_PREFIX is available
if [ -z "$BREW_PREFIX" ]; then
    echo "BREW_PREFIX is not set. Please run this script through brew.sh."
    exit 1
fi

# Install a modern version of Bash.
#brew install bash
#brew install bash-completion2

# Switch to using brew-installed bash as default shell
#if ! fgrep -q "${BREW_PREFIX}/bin/bash" /etc/shells; then
#  echo "${BREW_PREFIX}/bin/bash" | sudo tee -a /etc/shells;
#  chsh -s "${BREW_PREFIX}/bin/bash";
#fi;

# Install shell utilities
brew install rlwrap  # Wrapper to provide readline features to command line apps
brew install ssh-copy-id  # Add SSH keys to a server's authorized_keys file

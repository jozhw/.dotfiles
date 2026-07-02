#!/usr/bin/env bash
#
# Shell — interactive-shell helpers and conveniences.
#
# Add here: shell replacements/upgrades, completion, prompts, readline wrappers,
# and small helpers that improve the interactive terminal (ssh key helpers, etc.).

# Ensure BREW_PREFIX is available (set by brew.sh).
if [ -z "$BREW_PREFIX" ]; then
    echo "BREW_PREFIX is not set. Please run this script through brew.sh."
    exit 1
fi

# Optional: install a modern Bash and make it the default shell (disabled).
#brew install bash
#brew install bash-completion2
#if ! fgrep -q "${BREW_PREFIX}/bin/bash" /etc/shells; then
#  echo "${BREW_PREFIX}/bin/bash" | sudo tee -a /etc/shells;
#  chsh -s "${BREW_PREFIX}/bin/bash";
#fi;

brew install rlwrap        # Add readline editing/history to any command-line program
brew install ssh-copy-id   # Install your SSH key into a server's authorized_keys

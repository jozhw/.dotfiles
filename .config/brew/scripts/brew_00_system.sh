#!/usr/bin/env bash
#
# System — newer GNU/updated builds of the core command-line tools that ship
# outdated (or GPL-frozen) with macOS. Runs first so everything after it can
# rely on modern coreutils/findutils/etc.
#
# Add here: drop-in replacements for built-in Unix utilities (coreutils, sed,
# grep, find, ssh, tar, ...) and low-level libraries the base system needs.
# Do NOT put application-specific or single-purpose CLI tools here — those go in
# the brew_10_*.sh (cli) or a more specific bucket.

# Ensure BREW_PREFIX is available (set by brew.sh).
if [ -z "$BREW_PREFIX" ]; then
    echo "BREW_PREFIX is not set. Please run this script through brew.sh."
    exit 1
fi

# GNU core utilities (ls, cp, date, ...). Add
# "$(brew --prefix coreutils)/libexec/gnubin" to $PATH to use them unprefixed.
brew install coreutils
ln -s "${BREW_PREFIX}/bin/gsha256sum" "${BREW_PREFIX}/bin/sha256sum"

brew install moreutils      # Extra Unix tools such as `sponge`, `ts`, and `vidir`
brew install findutils      # GNU find/locate/updatedb/xargs (installed `g`-prefixed)
brew install grep           # GNU grep, newer than the bundled build (use as `ggrep`)
brew install openssh        # Up-to-date OpenSSH client and server
brew install screen         # Terminal multiplexer, newer than the bundled version
brew install gmp            # GNU multiple-precision arithmetic library
brew install pinentry-mac   # GNUPG passphrase reader

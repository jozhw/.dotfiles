#!/usr/bin/env bash
#
# brew.sh — wrapper that installs every Homebrew package in this directory.
#
# Do NOT rename this file: it is the entry point. It updates Homebrew, then
# sources each brew_*.sh script (in filename order) so their `brew install`
# calls run in the current shell. That is why BREW_PREFIX, exported below, is
# visible to every category script.
#
# Category scripts are named brew_<NN>_<category>.sh. The leading number groups
# them and fixes the install order (lower runs first); the category names the
# bucket. Ranges in use:
#
#   00-09  system   — GNU/updated replacements for built-in macOS tools
#   10-19  cli       — files, search, compression, shell, data pipelines
#   20-29  dev       — languages, LSPs, formatters, version control
#   30-39  media     — audio/video, graphics, documents
#   40-49  network   — web browsing and downloading
#   50-59  security  — CTF / pentest / forensics tooling
#   60-69  apps      — GUI applications (Homebrew casks)
#   70-79  misc      — everything else (personal info management, etc.)
#
# To add a package, edit the matching brew_<NN>_<category>.sh file (see the
# header in each for what belongs there) — or add a new brew_<NN>_<category>.sh
# and it is picked up automatically on the next run.

# Update and upgrade Homebrew first.
brew update
brew upgrade

# Save Homebrew's install location for the category scripts to reference.
export BREW_PREFIX=$(brew --prefix)

# Run every brew_<NN>_<category>.sh script in this directory, in filename order.
for script in brew_*.sh; do
    echo "Running $script..."
    source "$script"
done

# Remove outdated downloads to free up space.
brew cleanup

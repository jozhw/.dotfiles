#!/usr/bin/env bash

# Run all brew_<name>.sh scripts.

# Update and upgrade Homebrew first
brew update
brew upgrade

# Save Homebrew’s installed location
export BREW_PREFIX=$(brew --prefix)

# Loop through all brew_<name>.sh files and execute them
for script in brew_*.sh; do
    echo "Running $script..."
    source "$script"
done

# Cleanup outdated versions
brew cleanup

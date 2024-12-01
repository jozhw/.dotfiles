#!/usr/bin/env bash


# Install file management tools
brew install rename  # Perl-powered file rename script
brew install tree  # Display directories as trees
brew install vbindiff  # Visual binary diff

# Install GNU Stow for symbolic linking of dotfiles to $HOME dir.
brew install stow

# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed

# For pdf viewing
brew install poppler automake pkg-config

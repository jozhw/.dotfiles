#!/usr/bin/env bash
#
# Files — file and text tooling: managing, searching, diffing, transforming.
#
# Add here: file managers/renamers, directory listers, search/grep-likes,
# diff/hex-diff tools, stream editors, and dotfile linkers. Keep GNU drop-in
# replacements for built-in tools in brew_00_system.sh instead.

brew install rename      # Perl-based batch/regex file renamer
brew install tree        # Print directory contents as an indented tree
brew install vbindiff    # Interactive visual binary (hex) diff
brew install stow        # GNU Stow — symlink dotfiles into $HOME
brew install gnu-sed     # GNU sed, more capable than BSD sed (use as `gsed`)
brew install ack         # grep-like search tool optimized for source code

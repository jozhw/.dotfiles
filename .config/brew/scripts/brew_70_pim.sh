#!/usr/bin/env bash
#
# PIM — personal information management, and other odds and ends.
#
# Add here: finance/accounting, notes, todo, calendar, and contacts tools — the
# catch-all bucket for CLI tools that don't fit a category above.

brew install ledger    # Double-entry, plain-text accounting from the command line

# Search backend for Emacs' xeft, which does search-as-you-type over the
# Obsidian vault (see the xeft block in Emacs.org). xeft needs a dynamic module
# built against this library; the first `M-x xeft' offers to compile it, which
# also needs a C++ compiler and make. Without xapian that prompt fails with
# "Cannot start xeft because required dynamic module is missing".
brew install xapian    # C++ search engine library

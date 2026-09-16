# Changelog

All notable changes to this project will be documented in this file.

## [1.1.0] - 2026-09-16

### Added

- A single-file Org Agenda workflow with daily capture, state history, and yearly datetree archives.
- Obsidian, Xeft, ECA, R language-server, Pandoc, and Emacs 31 configuration support.
- Modular, ordered Homebrew package scripts; NAS rsync helpers; and Restic backup jobs.
- A generated Starlight documentation site sourced from the literate Emacs configuration.

### Changed

- Migrated Emacs package management to `straight.el` and simplified Denote metadata.
- Reorganized Homebrew packages, documentation, and archived legacy configuration.
- Made project Makefiles responsible for LaTeX builds instead of compiling `.tex` files on save.
- Kept the selected Emacs theme fixed during a session instead of rotating it hourly.

### Fixed

- Avoided conflicts between built-in and externally installed Emacs packages.
- Made tree-sitter grammar installation resilient when an individual grammar fails.
- Corrected Org paths, startup directory creation, and theme selection behavior.
- Updated tree-sitter detection for the Emacs 31 API.
- Prevented nested Restic locks from skipping or deadlocking scheduled jobs.
- Made dotfile synchronization portable to macOS and safe for non-main branches.

## [1.0.0] - 2026-01-10

Initial release.

[1.1.0]: https://github.com/jozhw/.dotfiles/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/jozhw/.dotfiles/releases/tag/v1.0.0

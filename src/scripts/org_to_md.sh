#!/bin/bash
#
# Regenerate the Starlight docs from the literate Org configuration.
# Run from the repository root.
#
# Note: the project README is now maintained directly as README.md (see the
# "readme.org -> readme.md" refactor), so only Emacs.org is converted here.

set -euo pipefail

# convert Emacs.org into the per-page Starlight docs (pruning stale pages)
python src/scripts/org_to_md.py Emacs.org -o docs/src/content/docs/emacs --clean

#!/usr/bin/env bash

set -euo pipefail

# obtained from https://systemcrafters.net/managing-your-dotfiles/using-gnu-stow/

# Sync dotfiles repo and ensure that dotfiles are tangled correctly afterward

GREEN='\033[1;32m'
BLUE='\033[1;34m'
RED='\033[1;31m'
NC='\033[0m'

# Resolve this script even when GNU Stow exposes it through a symlink.  macOS
# `readlink` has no `-f`, so resolve one link at a time.
SOURCE=${BASH_SOURCE[0]}
while [[ -L "$SOURCE" ]]; do
    SOURCE_DIR=$(cd -P "$(dirname "$SOURCE")" && pwd)
    SOURCE=$(readlink "$SOURCE")
    [[ "$SOURCE" != /* ]] && SOURCE="$SOURCE_DIR/$SOURCE"
done
DOTFILES_DIR=$(cd -P "$(dirname "$SOURCE")/.." && pwd)
cd "$DOTFILES_DIR"

if [[ $(git branch --show-current) != main ]]; then
    printf '%b\n' "${RED}Refusing to sync: switch to the main branch first.${NC}" >&2
    exit 1
fi

printf '%b\n' "${BLUE}Stashing existing changes...${NC}"
stash_result=$(git stash push -m "sync-dotfiles: Before syncing dotfiles")
needs_pop=1
if [[ "$stash_result" == "No local changes to save" ]]; then
    needs_pop=0
fi

printf '%b\n' "${BLUE}Pulling updates from dotfiles repo...${NC}"
echo
git pull --ff-only origin main
echo

if (( needs_pop == 1 )); then
    printf '%b\n' "${BLUE}Popping stashed changes...${NC}"
    echo
    git stash pop
fi

unmerged_files=$(git diff --name-only --diff-filter=U)
if [[ -n "$unmerged_files" ]]; then
   printf '%b\n' "${RED}The following files have merge conflicts after popping the stash:${NC}"
   echo
   printf '%s\n' "$unmerged_files"
else
   # Run stow to ensure all new dotfiles are linked
   stow .
fi

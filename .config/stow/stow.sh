#!/usr/bin/env bash

DOTFILES_DIR="$HOME/.dotfiles"

# Check if the .dotfiles directory exists
if [ ! -d "$DOTFILES_DIR" ]; then
  echo "Directory $DOTFILES_DIR does not exist. Exiting."
  exit 1
fi

cd "$DOTFILES_DIR" || exit

# Function to stow items
stow_items() {
  local dry_run=$1
  local stow_opts="-v --target=$HOME"
  [ "$dry_run" -eq 0 ] && stow_opts+=" --no"

  echo "${dry_run:+Dry run: }Stowing dotfiles..."
  stow $stow_opts .
}

# Get user input if they want to run a dry run or not
read -p "Would you like to run a dry run? Enter 0 for yes and 1 for no: " input

if [ "$input" -eq 0 ]; then
  echo "Running dry run of stow script"
  stow_items 0
elif [ "$input" -eq 1 ]; then
  echo "Running stow script"
  stow_items 1
else
  echo "Invalid input. Exiting."
  exit 1
fi

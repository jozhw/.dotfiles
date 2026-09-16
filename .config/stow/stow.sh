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
  local -a stow_opts=(-v --target="$HOME")
  if (( dry_run == 0 )); then
    stow_opts+=(--no)
    echo "Dry run: stowing dotfiles..."
  else
    echo "Stowing dotfiles..."
  fi

  stow "${stow_opts[@]}" .
}

# Get user input if they want to run a dry run or not
read -r -p "Would you like to run a dry run? Enter 0 for yes and 1 for no: " input

case "$input" in
  0) stow_items 0 ;;
  1) stow_items 1 ;;
  *) echo "Invalid input. Exiting."; exit 1 ;;
esac

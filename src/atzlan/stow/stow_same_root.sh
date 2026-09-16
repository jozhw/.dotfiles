#!/bin/bash

# Set the Stow directory
STOW_DIR="atzlan"

# Set the target bin directory in the root directory
TARGET_BIN_DIR=".bin"

# List of script files to include (full paths)
INCLUDE_SCRIPTS=(
    "rsync/rsync_macos_local.sh"
    "yt-dlp/yt_dlp.sh"
)

# Ensure the target bin directory exists
mkdir -p "$TARGET_BIN_DIR"

# Function to stow symbolic links
stow_scripts() {
    local target_dir="$1"
    shift
    for script_file in "$@"; do
        script_name=$(basename "$script_file")
        stow -d "$STOW_DIR" -t "$target_dir" -S "$(dirname "$script_file")" &&
        if [[ -f "$target_dir/$script_name" && ! -x "$target_dir/$script_name" ]]; then
            chmod +x "$target_dir/$script_name"
            echo "File $script_name was not executable and has been set to executable."
        fi
    done
}

# Stow script files
stow_scripts "$TARGET_BIN_DIR" "${INCLUDE_SCRIPTS[@]}"

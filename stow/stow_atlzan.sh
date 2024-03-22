#!/bin/bash

# Set the Stow directory
STOW_DIR="/"

# Set the target bin directory
TARGET_BIN_DIR="./.bin"

# Package name
PACKAGE="atzlan"

# List of paths to include
INCLUDE_PATHS=(
    "atzlan/rsync/rsync_macos_local.sh"
    "atzlan/yt-dlp/yt_dlp.sh"
)

# Ensure the target bin directory exists
mkdir -p "$TARGET_BIN_DIR"

# Loop through each path and stow it
for path in "${INCLUDE_PATHS[@]}"; do
    stow -R -t "$TARGET_BIN_DIR" -d "$STOW_DIR" "$path"
    
    # Check if the path is a script and not executable
    if [[ -f "$STOW_DIR/$path" && ! -x "$STOW_DIR/$path" ]]; then
        chmod +x "$STOW_DIR/$path"
        echo "File $path was not executable and has been set to executable."
    fi
done

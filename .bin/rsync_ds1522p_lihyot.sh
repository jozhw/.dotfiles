#!/bin/bash
set -euo pipefail

# ===== Config =====
SRC_DIR="$HOME/Library/CloudStorage/ProtonDrive-john@winterblossom.io-folder/Lihyot"
REMOTE_USER="jozhw"
REMOTE_HOST="ds1522p.wu.ventures"
REMOTE_BASE="/volume1/Trove/Lihyot/"

# ===== Date-based naming =====
YEAR="$(date +%Y)"
MMDD="$(date +%m%d)"
REMOTE_DIR="${REMOTE_BASE}/"

# ===== Ask user =====
read -r -p "Dry-run? Enter 0 for yes and 1 for no: " input

DRYRUN_FLAG=""
if [[ "$input" == "0" ]]; then
  echo "Running DRY RUN of rsync"
  DRYRUN_FLAG="--dry-run"
elif [[ "$input" == "1" ]]; then
  echo "Running LIVE rsync"
else
  echo "No command selected"
  exit 1
fi

# ===== Ensure source exists =====
if [[ ! -d "$SRC_DIR" ]]; then
  echo "Source directory not found: $SRC_DIR"
  exit 1
fi

# ===== Ensure destination exists on Synology =====
ssh "${REMOTE_USER}@${REMOTE_HOST}" "mkdir -p \"${REMOTE_DIR}\""

# ===== Rsync =====
# Notes:
# -aPv = archive + progress + verbose
# --no-times avoids timestamp preservation weirdness with some mounts
# --append-verify is helpful if large files can be interrupted; otherwise you can remove it
rsync \
  -aPv \
  --no-times \
  --append-verify \
  --delete \
  --exclude='**/.DS_Store' \
  $DRYRUN_FLAG \
  "${SRC_DIR}/" \
  "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}"

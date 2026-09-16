#!/bin/bash
set -euo pipefail

# Ensure no overlap with backup/check (wait until lock is available)
exec 9>/tmp/restic-job.lock
flock 9

DOCKER_BIN="/usr/local/bin/docker"   # verify with: which docker
CONTAINER="restic"

LOG_DIR="/volume2/docker/restic/logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/restic-prune-$(date +%F_%H-%M-%S).log"

# Apply retention and reclaim space
stdbuf -oL -eL "$DOCKER_BIN" exec "$CONTAINER" restic forget \
  --keep-daily 30 \
  --keep-weekly 8 \
  --keep-monthly 120 \
  --keep-yearly 20 \
  --prune \
  2>&1 | tee -a "$LOG_FILE"

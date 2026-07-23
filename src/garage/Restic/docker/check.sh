#!/bin/bash
set -euo pipefail

# Prevent overlapping restic jobs (backup/prune/check)
# For check, we WANT to wait until backup/prune finishes
exec 9>/tmp/restic-job.lock
flock 9

DOCKER_BIN="/usr/local/bin/docker"   # verify with: which docker
CONTAINER="restic"

LOG_DIR="/volume2/docker/restic/logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/restic-check-$(date +%F_%H-%M-%S).log"

# Run repository integrity check
stdbuf -oL -eL "$DOCKER_BIN" exec "$CONTAINER" restic check \
  2>&1 | tee -a "$LOG_FILE"

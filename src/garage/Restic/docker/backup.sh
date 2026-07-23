#!/bin/bash
set -euo pipefail

# Prevent overlapping restic jobs (backup/prune/check)
exec 9>/tmp/restic-job.lock
flock -n 9 || { echo "Another restic job is running; skipping backup"; exit 0; }

DOCKER_BIN="/usr/local/bin/docker"   # change if `which docker` shows a different path
CONTAINER="restic"

LOG_DIR="/volume2/docker/restic/logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/restic-backup-$(date +%F_%H-%M-%S).log"

# Run backup (line-buffered output + append to log)
stdbuf -oL -eL "$DOCKER_BIN" exec "$CONTAINER" restic -vv backup /data \
  --exclude-file /excludes.txt \
  2>&1 | tee -a "$LOG_FILE"

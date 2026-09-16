#!/bin/bash
set -euo pipefail

JOB="${1:-}"

# Each target script owns the shared lock.  Locking here as well would make
# the child contend with its parent: backup would skip and prune/check would
# wait forever.
case "$JOB" in
  backup) /volume2/docker/restic/backup.sh ;;
  prune)  /volume2/docker/restic/prune.sh ;;
  check)  /volume2/docker/restic/check.sh ;;
  *) echo "Usage: $0 {backup|prune|check}"; exit 2 ;;
esac

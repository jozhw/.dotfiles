#!/bin/bash
set -euo pipefail

exec 9>/tmp/restic-job.lock
JOB="${1:-}"

# backup: skip if busy; prune/check: wait
if [[ "$JOB" == "backup" ]]; then
  flock -n 9 || exit 0
else
  flock 9
fi

case "$JOB" in
  backup) /volume2/docker/restic/backup.sh ;;
  prune)  /volume2/docker/restic/prune.sh ;;
  check)  /volume2/docker/restic/check.sh ;;
  *) echo "Usage: $0 {backup|prune|check}"; exit 2 ;;
esac

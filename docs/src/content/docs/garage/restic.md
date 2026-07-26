---
title: Restic 
description: Backup stuff 
---


## Initialization

```
docker exec restic restic init
```

then test make sure the following does no error
```
docker exec restic restic snapshots
```

```
docker exec restic sh -lc 'ps aux | grep -v grep | grep -E "restic|forget|prune|check|backup" || echo "No restic-related process running"'
```




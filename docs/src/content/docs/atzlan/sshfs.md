---
title: sshfs
description: Documentation related to the usage of sshfs.
---

sshfs is used to file mount remote files or directories. This is especially useful for development since I may not have access to lsp for certain servers.

For macos it is important to download [MacFuse](https://macfuse.github.io/) to enable mounting.

## Mounting command

```bash
sshfs user@remote_host:/path/to/repo ~/remote_repo
```

## Unmount command

```bash
fusermount -u ~/remote_repo  # Linux
umount ~/remote_repo        # macOS
```

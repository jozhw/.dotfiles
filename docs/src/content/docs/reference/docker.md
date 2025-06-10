---
title: Docker
description: A page relates to docker configurations and usage (generally) in this repository.
---

This page serves to contain all general Docker configurations or any insights related to Docker that would be useful for development.

## *docker-compose* containers

This repository uses `docker-compose` religiously, hence it is important to retain a reference of commands and etc. that could help me maximize the utility I get from `docker-compose`.

### Viewing

To view active containers, you must be in the root directory of where the `docker-compose` command was called and run the following,

```shell
    docker-compose ps
```

## Accessing CLI

`i` means interactive and `t` means terminal.

```shell
    docker exec -it <container_name> /bin/bash
```


---
title: Networking Services
description: Networking documentation and help
---

This document contains the networking services I use or have used along with configurations and references that may be useful for setting up or troubleshooting.

## Tsdproxy

`tsdproxy` is used as a proxy server for services hosted on the *Tailscale* network. 

### Setup

Must define a volume named `tsdproxydata` using the following shell command to be used:

```shell
  docker volume create tsdproxydata
```

An alternative is to create a file in the file system which would be preferred. If that is the case then make sure you specify a `./` to indicate that it is a file on the filesystem. 

### Labels

For `tsdproxy` to work, you must pass label variables in the `docker-compose.yaml`.

```yaml
  tsdproxy.enable: "true"
  tsdproxy.name: "<service_name>"
```
`tsdproxy` will bind to the docker socket of the host machine to find containers with the `tsdproxy.*` labels to create a machine on the *Tailscale* network.

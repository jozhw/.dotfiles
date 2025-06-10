---
title: Media Services
description: Media services configuration and help.
---


## Navidrome

Navidrome is an open source self-hosted  music server akin to the services Spotify provides.

### Troubleshooting

**Not all Tracks Appearing in Album**

This is usually do to the fact that navidrome scanned that directory already and after naidrome has scanned, it will not scan again — may have to do with keeping things smooth and fast. Thus, you should run the following command with the container still running so that it will force scan the entire library.

```shell
    sudo docker-compose exec navidrome /app/navidrome scan -f
```




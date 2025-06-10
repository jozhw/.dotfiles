---
weight: 1
bookFlatSection: true
title: "CLI Tools"
---

# Introduction

Houses all related to cli tools that I use

## rsync


### `Operation not permitted (1)`

This error seems to indicate that there is a permission issue. Firstly make sure that the user used for ssh has the correct permissions. To set the user use `sudo chown -R <user_name>`.

Then you see if you have read and write permissions. To see current permissions run `ls -ld <path>`. To change the permissions run 

```shell
    sudo chmod u+rwx <path>
```

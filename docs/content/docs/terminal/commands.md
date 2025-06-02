---
weight: 1
bookFlatSection: true
title: "Commands"
---

# Introduction

The purpose of this file is to keep a document with essential command line (unix) commands in a convenient area for future reference.


# Keyboard Shortcuts

- **Clearing:** `command` + `K` 

- **Auto-fill/Completion:** `tab`

- **Navigation:** `control` + ...

    - **Beginning of line:**  `a`

    - **End of the line:** `e`

- **Deletion:** `control` + ...

    - **On cursor:** `d`

    - **Before cursor:** `u`

    - **From cursor to end of the line:** `k`

    - **Word immediately before cursor:** `w`

- **Switch places of words:** `esc` + `t`

- **Drop cursor at mouse:** hold `option` + move `cursor`

- **Find/Search:** `command` + `F`

- **Previous command:** `↑`

- **Earliest command/reverse:** `↓`

# MacOS Specific Keyboard Shortcuts

- **Enlarge window:** `command` + `+`

# Commands


## Dired Informational

Current working directory.

```shell
  pwd
```

Listing files

```shell
  ls
  # flags
  # - l: long format
  # - a: hidden files
  # - h: hidden files
  # - G: color coat
  # - F: add wax to back off older
  ls -lahGF
```

## Clearing

```shell
  clear
```

## Informational

Disk usage

```shell
  du
  # flags
  # - s: size of each individual file
  # - h: human readable
  du -sh
```

Gets the command history.

```shell
  history
```

Manual for packages.

```shell
  man
```
## File Manipulation

Reading the contents of a file w/o opening

```shell
  cat
```

Word counter

```shell
  wc
  # flags
  # - c: word count for a file
  # - l: number of lines
```

## Networking

Find ip address (en0 if vpn also vton).

```shell
  ifconfig
```

Ping to see if you are connected.

```shell
  ping
```


Finding the host, more information about the site

```shell
  host
```

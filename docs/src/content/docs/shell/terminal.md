---
title: Terminal
description: Terminal help
---

## Useful Commands

**Navigation**

- `pwd`, get the current working directory.
- `ls`, listing files. Can add the following flags: `l` (long format), `a` (hidden files), `h` (human readable), `G` (color coat), `F` (add wax to back off older).

**Info**

- `du` (disk usage). Can add the following flags: `s` (size of each individual file), `h` (human readable).
- `history`. Get the history of commands
- `man` (manual).
- `wc` (word counter).

**Networking**

- `ifconfig`, use to find ip address.
- `ping`
- `host`

## Keyboard Shortcuts

- Clearing: `command` + `K` 

- Navigation: `control` + ...

    - Beginning of line: `a`

    - End of the line: `e`

- Deletion: `control` + ...

    - On cursor: `d`

    - Before cursor: `u`

    - From cursor to end of the line: `k`

    - Word immediately before cursor: `w`

- Switch places of words: `esc` + `t`

- Drop cursor at mouse: hold `option` + move `cursor`

- Find/Search: `command` + `F`

- Previous command: `↑`

- Earliest command/reverse: `↓`




## Privileges

Usually an error that is worded "*Operation is not permitted (1)*" is indicative of a user privilege or permission issue. Firstly make sure that the user used for ssh has the correct permissions. To set the user use `sudo chown -R <user_name>`.

Then you see if you have read and write permissions. To see current permissions run `ls -ld <path>`. To change the permissions run 

```shell
    sudo chmod u+rwx <path>
```




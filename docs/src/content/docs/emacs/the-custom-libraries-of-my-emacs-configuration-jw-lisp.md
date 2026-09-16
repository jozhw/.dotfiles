---
title: "The custom libraries of my Emacs configuration (`jw-lisp/`)"
description: "Currently empty --- jw-copy.el lived here until its two commands were found"
---

Currently empty --- `jw-copy.el` lived here until its two commands were found
to duplicate built-ins: `jw-copy-file` wrapped `copy-file` (already interactive,
and it **prompts** on an existing destination rather than refusing), and
`jw-dired-get-file-path` duplicated `dired-copy-filename-as-kill` (`w` in Dired,
`C-u 0 w` for the absolute path). Its `C-c p` binding in `dired-mode-map` also
shadowed the global `C-c p` for `cursory-set-preset` inside Dired buffers.

The `load-path` entry above is kept so this directory is ready for future
libraries.

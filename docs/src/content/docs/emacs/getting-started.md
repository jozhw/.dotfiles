---
title: "Getting Started"
description: "This org file is used to set up my emacs configuration. The code blocks of this org file"
---

This org file is used to set up my emacs configuration. The code blocks of this org file
will be executed using `org-babel-tangle`. Thus, to initiate you must use `M-x ^org-babel-tangle` or use the key-binding `C-c C-v t`.

## Dependencies

### aporetic typeface

The GUI version of Emacs uses this typeface, so make sure to have aporetic installed. Here is the [repo](https://github.com/protesilaos/aporetic) for the install instructions.

For macos users, you can simply run

```bash
brew install font-aporetic
```

## Running Emacs

To run emacs in the command-line/terminal simply use the following command while in your command-line/terminal: `emacs -nw`.

## Convention: the `lexical-binding` header block

Every file tangled from this document opens with a one-line block of its own:

```emacs-lisp
;;; jw-emacs-foo.el --- One-line summary -*- lexical-binding: t; -*-
```

Emacs reads the `-*- ... -*-` prop line only from **line 1**, and only if line 1 starts
with `;` — a single leading space silences the cookie and the file loads with dynamic
binding. A cookie in `early-init.el` does not carry over; `lexical-binding` is
per-file.

The reason for the dedicated block is that `org-babel-tangle` strips each block's
**minimum** indentation, computed per block. When the cookie shares a block with other
code, one line at column 0 anywhere in that block (a flush-left `(provide ...)`, say)
makes the minimum zero, nothing is stripped, and the header keeps its leading spaces.
A block holding only the cookie is always its own minimum, so it always lands at
column 0 regardless of what the rest of the file looks like.

When adding a new tangled file, give it a header block like the above, placed before
any other block targeting that file.

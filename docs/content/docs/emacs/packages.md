---
weight: 1
bookFlatSection: true
title: "Packages"
---


# Introduction

This document serves to aide with setting up any packages along with addressing any issues faced when implementing the `emacs` packages.

# `magit.el`
## Commands

**Global Commands**

- `C-x g` (`magit-status`): Open Magit status buffer.
- `C-c M-g` (`magit-dispatch-popup`): Open Magit command popup.
- `q` (`magit-mode-quit-window`): Close Magit buffer.
- `C-c M-c` (`magit-commit`): Start commit process.
- `C-c M-p` (`magit-push-popup`): Push changes.
- `C-c M-f` (`magit-pull-popup`): Pull changes.
- `C-c M-b` (`magit-branch-popup`): Manage branches.

**Status Buffer**

- `TAB`: Expand/collapse section.
- `S` (`magit-stage-item`): Stage changes.
- `U` (`magit-unstage-item`): Unstage changes.
- `c` (`magit-commit-popup`): Commit changes.
- `P` (`magit-push-popup`): Push changes.
- `F` (`magit-pull-popup`): Pull changes.
- `b` (`magit-branch-popup`): Manage branches.
- `l l` (`magit-log`): View commit log.
- `r` (`magit-refresh`): Refresh status buffer.
- `d` (`magit-diff-popup`): Show changes.

**Diff Buffer**

- `n` (`magit-section-forward`): Move to the next hunk.
- `p` (`magit-section-backward`): Move to the previous hunk.
- `s` (`magit-stage`): Stage current hunk.
- `u` (`magit-unstage`): Unstage current hunk.
- `C-c C-a` (`magit-diff-show-or-scroll-up`): Scroll up in diff buffer.
- `C-c C-e` (`magit-diff-show-or-scroll-down`): Scroll down in diff buffer.

**Log Buffer**
- `l l` (`magit-log`): Refresh log buffer.
- `RET`: Show details of the commit at point.
- `d` (`magit-diff-visit-file`): Show changes for the commit at point.
- `TAB`: Toggle commit details.

**Commit Buffer**
- `C-c C-c` (`magit-commit`): Finish the commit.
- `C-c C-a` (`magit-commit-ammend`): Amend the last commit.

**Branch Popup**
- `b c` (`magit-branch-create`): Create a new branch.
- `b k` (`magit-branch-delete`): Delete a branch.
- `b m` (`magit-branch-move`): Rename a branch.
- `b r` (`magit-branch-rename`): Rename the current branch.
- `b c` (`magit-branch-checkout`): Checkout a branch.

## Configurations

### GPG Signing

When on the commit buffer, the argument for `gpg-signing` or `-S` may not be displayed. To resolve this issue manually, on the commit buffer menu, you must enter transient mode with `C-x l` and follow the prompting from there by typing the argument that you want to change the layering and then set the layering.




# `org-mode`

## Commands

- `SHIFT-TAB`: To change the view to outline or full view use repeatedly for the desired visual look.

- `TAB`: To condense or uncondense content under a heading, make sure the cursor is on the same line as the heading and use `TAB` to cycle through uncondensing or condensing the content.

## `org-latex`

[Straightforward Emacs guide](https://jakebox.github.io/youtube/org_latex_video.html). The following is copied from that guide.

**Create & include a `SETUPFILE`:**

A `SETUPFILE` is simply a file that contains some sort of org mode configuration. For example, the `#+LATEX_CLASS:` we saw earlier. Or, maybe `#+LATEX_HEADER:`. It could even be as simple as `#+TITLE:`. In this `SETUPFILE` we can place all of our lines of LaTeX code we want included in the final generated document. This is a very powerful and easy method of perfectly customizing our export.

Linking the `SETUPFILE`:

```
#+SETUPFILE: ~/Dropbox/Mackup/emacs-stuff/org/jake-standard-latex-export.org
```

**Structure the `SETUPFILE`:**

Now, in that `SETUPFILE`, we place the LaTeX lines we normally would put in a `.tex` file. For example, `\usepackage{amsmath}`. Hopefully you have your own favorite `.tex` file setup, but if not I suggest learning LaTeX and/or finding a template online.

I suggest using a macro to add `#+LATEX_CLASS:` to each line, rather than typing it manually of course! Notice the `LaTeX_CLASS:` - the custom one we just created.

```
#+LaTeX_CLASS: org-plain-latex
#+LaTeX_CLASS_OPTIONS: [letter]
#+LATEX_HEADER: \usepackage{lmodern} % Ensures we have the right font

#+LATEX_HEADER: \usepackage[AUTO]{inputenc}
#+LATEX_HEADER: \usepackage{graphicx}
#+LATEX_HEADER: \usepackage{amsmath, amsthm, amssymb}
#+LATEX_HEADER: \usepackage[table, xcdraw]{xcolor}

% and so on...
```

**Extra Notes:**

Some notes not covered in the video.

Enable using `listings` for code highlighting

```emacs-lisp
  (setq org-latex-listings 't)
```

In your `SETUPFILE:`

```
#+LATEX_HEADER: \usepackage{listings}
```

If you enable `org-latex-listings` but don’t include the `listings` package in your TeX file, it will likely not compile (if you are exporting an org file with code blocks).

**Useful commands:**

- `org-latex-preview`: Toggles preview LaTeX math at point


## `org-transclude.el`

Link to the docs: [org-transclude](https://nobiot.github.io/org-transclusion/)


## `org-todo.el` and `org-agenda.el`

To create a percentage completion rate simply add `[/][%]` and then do the command `C-c C-c`.


# `package.el` and `use-package.el`

`package.el` and `use-package.el` are the default packages used by emacs to install other packages. 

## Troubleshooting

### Package Installation

If you are having trouble with package installation such as a package cannot be found but it is on melpa, then you will have to run `package-refresh-contents`.


# `pdf-tools.el`

## Troubleshooting


**Troubleshooting 2025-01-14: Does Not Work**

If you receive the option to rebuild the =epdfserver= and you agree to building on Emacs, there are instances where the build fails. When running =M-x pdf-tools-install= you will rebuild within Emacs and will obtain more information. If the error consists of not being able to find poppler, copy and paste the command used to run the installation and run it in the command line outside of emacs.

This solution seems to work currently as of 2025-01-14.

**Troubleshooting 2025-01-15: Works**

The issue with the **2025-01-14** is that if the installation works within the command line, when opening up a pdf file on Emacs would lead to the epdfserver crashing. This issue I found had to do with confict with `macports` being installed. If you uninstall macports, then the issue is resolved. 


# `vertico.el`

## Troubleshooting

### Errors

If in the Emacs buffer if you get a `Error in post-command-hook (vertico--exhibit): (void-function compat--completion-metadata-get)` error then you either delete vertico and recomplile or recompile

```
  M-x package-recompile
  vertico

```

The reason this issue is observed is due to updating Emacs.

A link to the github issue where I found the solution is [here](https://github.com/minad/vertico/discussions/501#discussioncomment-12390155).



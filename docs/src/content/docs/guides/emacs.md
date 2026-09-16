---
title: Emacs
description: Emacs information for quick reference
---

The vast majority of configuration help and troubleshooting is in the literate configuration file `Emacs.org`. This page is the shorter operational reference.

## Org Agenda

The agenda is deliberately a daily worklist, not a project-management system. It uses one permanent file:

```text
~/Core/Otzar/Docs/agenda/todo.org
```

### Daily workflow

1. Press `C-c c t` to capture a task for today.
2. Type the task and press `C-c C-c` to save the capture.
3. Press `C-c a a` to open today's agenda.
4. In the agenda, press `t` on a task to move it through `TODO`, `WAIT`, `DONE`, or `CANCEL`.

Captured tasks receive a `SCHEDULED` timestamp for today automatically. A task written manually without a schedule appears in the global TODO list (`C-c a t`), but not necessarily in the daily agenda.

Useful keys inside the agenda:

| Key | Action |
| --- | --- |
| `RET` | Visit the task in `todo.org` |
| `t` | Change its TODO state |
| `g` | Refresh the agenda |
| `f` / `b` | Move forward or backward one day |
| `l` | Toggle the log of tasks completed on the displayed day |
| `q` | Close the agenda |

`DONE` and `CANCEL` tasks disappear from the normal daily view immediately, but their timestamps remain in `todo.org`. Run `C-c A` occasionally to sweep finished tasks into yearly files under `~/Core/Otzar/Docs/agenda/archive/`. Those custom yearly archives are not automatically discovered by the agenda's `v A` command; open the relevant year directly or search that directory.

The full rationale and implementation are in the `org-agenda` section of `Emacs.org` and the generated **Editor → Modules** page.

## Packages

### org-latex

[Straightforward Emacs guide](https://jakebox.github.io/youtube/org_latex_video.html). The following is copied from that guide.

**Firstly create a setup file**,

A `SETUPFILE` is simply a file that contains some sort of org mode configuration. For example, the `#+LATEX_CLASS:` we saw earlier. Or, maybe `#+LATEX_HEADER:`. It could even be as simple as `#+TITLE:`. In this `SETUPFILE` we can place all of our lines of LaTeX code we want included in the final generated document. This is a very powerful and easy method of perfectly customizing our export.

Linking the `SETUPFILE`:

```
#+SETUPFILE: ~/Dropbox/Mackup/emacs-stuff/org/jake-standard-latex-export.org
```

**Secondly, structure the `SETUPFILE`**,

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

**Some extra notes**,

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

**Some useful commands**,

- `org-latex-preview`: Toggles preview LaTeX math at point


## Useful Commands

These commands consists of third-party and native commands. Since it is my configuration it is adapted to suit my needs.

### Native Emacs

**Code Formatting**

`C-x C-;` (*comment*), comment or uncomment the current line (comment-line). If the region is active, comment or uncomment the lines in the region instead.                                                                                            

`C-x TAB` (*indent*), Use the arrow keys (← or →) to indent interactively and then press `esc` three times to finish indenting.


**Navigation**

`C-x d` (*dired-mode*), enter dired mode.

`C-x C-q`(*write dired-mode*), enter Wdired mode (write dired). In this mode, you can edit file names just like you would any file. However, when you have completed the edits of the dired, make sure to run the command `C-c C-c` to finish.

`C-c C-o` (*open link*), when the cursor is over a link, this command will open the link

`C-c C-l`(*edit link*), when the cursor is over a link, this command will open the link in a minibuffer for editing.

**Writing**

`C-x 8 RET` (*special symbols*), to access special symbols.

### `org-todo.el` and `org-agenda.el`

To create a percentage completion rate simply add `[/][%]` and then do the command `C-c C-c`.

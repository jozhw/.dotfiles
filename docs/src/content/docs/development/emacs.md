---
title: Emacs
description: Emacs information for quick reference
---

The vast majority of configuration help and troublshooting should be in the literate configuration file `Emacs.org`. The purpose of this document is for quick reference for troubleshooting (especially if not in the configuration file) and general help such as Emacs commands that I have trouble remembering or is not that intuitive.

## Packages



### `gptel.el`

Incorporates the use of llms in the emacs client. For a great summary of the features please see [Ben Simon's video](https://www.blogbyben.com/2024/08/gptel-mindblowing-integration-between.html). For accessing the source code please see [karthink's repo](https://github.com/karthink/gptel).


### `magit.el`

**GPG Signing**

When on the commit buffer, the argument for `gpg-signing` or `-S` may not be displayed. To resolve this issue manually, on the commit buffer menu, you must enter transient mode with `C-x l` and follow the prompting from there by typing the argument that you want to change the layering and then set the layering.



### `org.el`

#### org-latex

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

#### `org-transclude.el`

Link to the docs: [org-transclude](https://nobiot.github.io/org-transclusion/)

#### `org-todo.el` and `org-agenda.el`

To create a percentage completion rate simply add `[/][%]` and then do the command `C-c C-c`.




### `pdf-tools.el`

#### Troubleshooting

**Troubleshooting 2025-01-14: Does Not Work**

If you receive the option to rebuild the `epdfserver` and you agree to building on Emacs, there are instances where the build fails. When running `M-x pdf-tools-install` you will rebuild within Emacs and will obtain more information. If the error consists of not being able to find poppler, copy and paste the command used to run the installation and run it in the command line outside of emacs.

This solution seems to work currently as of 2025-01-14.

**Troubleshooting 2025-01-15: Works**

The issue with the **2025-01-14** is that if the installation works within the command line, when opening up a pdf file on Emacs would lead to the epdfserver crashing. This issue I found had to do with confict with `macports` being installed. If you uninstall macports, then the issue is resolved. 


### `use-package.el`

#### Troubleshooting

If you are having trouble with package installation such as a package cannot be found but it is on melpa, then you will have to run `package-refresh-contents`.

### `vertico.el`

#### Troubleshooting

If in the Emacs buffer if you get a `Error in post-command-hook (vertico--exhibit): (void-function compat--completion-metadata-get)` error then you either delete vertico and recomplile or recompile

```
  M-x package-recompile
  vertico

```

The reason this issue is observed is due to updating Emacs.

A link to the github issue where I found the solution is [here](https://github.com/minad/vertico/discussions/501#discussioncomment-12390155).




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

## Features

The features are implementations on top of the native emacs packages and not third-party packages.

### Sound Support

Usually this is a problem for macos and I found a snippet of code that enables sound support. The way to tell is by running `M-x play-sound-file` and navigating to the `.wav` file will ouput "This Emacs binary lacks sound support."

```emacs-lisp
  ;; on macos, fix "This Emacs binary lacks sound support" 
  ;; - https://github.com/leoliu/play-sound-osx/blob/master/play-sound.el
  ;; - update according to https://github.com/leoliu/play-sound-osx/issues/2#issuecomment-1088360638
  (when (eq system-type 'darwin)
    (unless (and (fboundp 'play-sound-internal)
                 (subrp (symbol-function 'play-sound-internal)))
      (defun play-sound-internal (sound)
        "Internal function for `play-sound' (which see)."
        (or (eq (car-safe sound) 'sound)
            (signal 'wrong-type-argument (list sound)))
      
        (cl-destructuring-bind (&key file data volume device)
            (cdr sound)
        
          (and (or data device)
               (error "DATA and DEVICE arg not supported"))
        
          (apply #'start-process "afplay" nil
                 "afplay" (append (and volume (list "-v" volume))
                                  (list (expand-file-name file data-directory))))))))

```


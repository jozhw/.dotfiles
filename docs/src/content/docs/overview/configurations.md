---
title: Configurations
description: Documentation for Configurations
---


Please see the `ConfigInfo.org` file for more detailing of particular configurations.

## emacs

All emacs configurations are written via a literate configuration in `org-mode` named `Emacs.org`. Each section in `Emacs.org` is modularized via `tangle` upon saving the file. All of the modularized configs are stored within the `.emacs.d` directory.

## elpa

The elpa directory contains all of the emacs packages that I use for my configuration. The reason why this is seperate and vc'd is to preserve the configuration for emacs, and to decrease the liklihood of errors in setting up. Furthermore, certain packages may have breaking changes that will be in need for fixes, which could be a major hassle. Thus, if a package is accidently updated, then it can be revered to the previous version.

## .profile

`.profile` serves as a generic shell configuration that will be applied in all shell sessions (bash or zsh).

To find which shell you are using, simply enter the command `echo %SHELL`.

To switch default shells, enter the command `chsh -s <PATH_TO_SHELL>`. The `<PATH_TO_SHELL>` typically is in the form of `/bin/bash` for `bash` and `/bin/zsh` for `zsh`.
   
## macos

The `.macos` file is used to configure macs. The template was taken from [@mathiasbynens](https:/*github.com*mathiasbynens*dotfiles*blob*main*.macos) and adjusted for my needs.

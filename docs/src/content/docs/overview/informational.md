---
title: Informational
description: Documentation for Informational
---


## Version Control

### Commit Syntax

To keep all of the commits organized, I suggest for my own reference to use the following commit syntax. The first is the action (i.e. fix, add, rm) and then the functionality that was changed (i.e. emacs, readme, stow) followed by the commit message.

```

  <ACTION>/<FUNCTIONALITY_CHANGED>: "MESSAGE HERE"

```

### Git LFS

The repo uses `git` and `git lfs` to keep versioning control. Make sure that you have `git lfs` installed (should be installed by the setup scripts). To see which files are being tracked by `git lfs`, check the `.gitattributes` file.

### Submodules

To see which files are being tracked by `git submodule`, see the `.gitmodules` file in the root directory.

The following paths are submodules:

#### `./.emacs.d/elpa`

Contains all of the emacs packages that I use.

The reason for this submodule is to avoid breaking changes on updates and can revert to previous versionings.

#### `./.emacs.d/fonts/`

This directory contains the fonts that I currently use and VC is linked to the repo for the respective fonts to faciliate updates.

##### `Iosevka`

Font face that I currently do not use, but I use Prot's updated version of the Iosevka typeface

##### `iosevka-comfy`

Font face that I use for **Emacs**.
  

---
title: Setting Up
description: Documentation for Setting Up
---


## brew

All of the brew scripts are within the brew dir. The path relative to this project's root directory is as follows: `./brew/scripts`.

### brew.sh

The script was heavily inspired from [mathiasbynens dotfiles github repo](https:/*github.com*mathiasbynens*dotfiles*blob*main*brew.sh), but adjusted to suit my needs. A notable difference between my "brew.sh" is that it is modularized with the brew.sh being the wrapper that will download all of the brew files.

Make sure that permissions are added by running `chmod +x brew.sh` in your command line.

To run the script:

```shell

  ./brew.sh

```

### brew_<category>.sh

The purpose of these scripts are to categorize brew installs. The wrapper, `brew.sh`, when executed will iterate through all of the `brew_<category>.sh` scripts within the same directory as the `brew.sh`. 

## stow

To stow (create symbolic links), after installing gnu stow, run the script in the `/.config/stow/stow.sh` script.

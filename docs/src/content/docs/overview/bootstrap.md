---
title: Bootstrapping a Machine
description: Setting up a new machine from these dotfiles.
sidebar:
  order: 2
---

The high-level path for a fresh machine: install packages with Homebrew, then symlink the configs into place with GNU Stow.

## 1. Install packages with Homebrew

All brew installs live in `.config/brew/scripts/`, split into category files. The wrapper `brew.sh` iterates over every `brew_<category>.sh` script in that directory, so you only run one command.

Make the wrapper executable and run it:

```shell
chmod +x brew.sh
./brew.sh
```

`brew.sh` is adapted from [mathiasbynens' dotfiles](https://github.com/mathiasbynens/dotfiles/blob/main/brew.sh), modularized so each category is its own file.

## 2. Symlink configs with Stow

After installing GNU Stow (it's in the brew lists), create the symlinks by running the stow script:

```shell
./.config/stow/stow.sh
```

Stow creates symbolic links from this repo into your home directory, so the tracked files become your live configuration.

## 3. macOS defaults (optional)

The `.macos` file applies macOS system preferences. It's based on [mathiasbynens' `.macos`](https://github.com/mathiasbynens/dotfiles/blob/main/.macos), adjusted to my needs. Review it before running — it changes a lot of system settings.

## Shell notes

`.profile` is a generic shell config applied across bash and zsh sessions. To check your current shell:

```shell
echo $SHELL
```

To change your default shell (e.g. to zsh):

```shell
chsh -s /bin/zsh
```

## Next steps

- Set up commit signing and other tooling — see [Setup](/shell/setup/).
- Regenerate or browse the editor config under **Emacs**.

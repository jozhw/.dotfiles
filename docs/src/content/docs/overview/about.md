---
title: About These Dotfiles
description: What this repository is, and what this documentation is for.
sidebar:
  order: 1
---

This repository holds my personal configuration (dotfiles), the services I run in my home lab, and a collection of small scripts. This site is where I document the parts that are **not** self-explanatory from the source.

## What lives in the repo

- **Editor** — a literate Emacs configuration in `Emacs.org`. Each section is tangled to `.emacs.d/` on save via `org-babel-tangle`.
- **Shell & CLI** — `.profile`, `.zshrc`, brew package lists, and `stow` for symlinking everything into place.
- **Homelab** (`src/garage/`) — Docker-based self-hosted services, each with its own directory and `docker-compose.yaml` for reproducibility.
- **Scripts** (`src/atzlan/`) — small, mostly macOS/bash utilities.

## What this documentation is (and isn't)

This is a personal reference, not a tutorial for others. The guiding rule is to **document what the source can't document itself**: operational runbooks, recovery steps, and cross-cutting troubleshooting that won't survive in memory. Where the config is already readable (the literate Emacs file, shell rc files), the docs only mirror or summarize it rather than duplicating it.

For the reasoning behind how this site is organized, see the `design/DESIGN_PHILOSOPHY.md` file in the repository root.

## Conventions

Commits follow a simple `<action>/<area>: message` syntax — for example `fix/emacs: correct theme hook` or `add/garage: immich runbook`. Common actions are `add`, `fix`, `rm`, `refactor`, and `update`.

The repo uses `git` with `git lfs` (see `.gitattributes`) and submodules (see `.gitmodules`).

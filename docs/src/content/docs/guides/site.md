---
title: Editing This Site
description: How this documentation site is built and regenerated.
---

This site is built with [Starlight](https://starlight.astro.build/), the official documentation theme by the Astro team. The pages under **Emacs** are generated from the literate `Emacs.org` config; everything else is hand-written.

## Regenerating the Emacs pages

The literate config is the source of truth. To regenerate the Emacs pages after editing `Emacs.org`, run from the repo root:

```shell
./src/scripts/org_to_md.sh
```

This converts `Emacs.org` into `docs/src/content/docs/emacs/*.md` (pruning any stale pages). Do **not** hand-edit files in `emacs/` — they are overwritten on every run.

## Development

Run a local dev server with hot reload (using `yarn`, from the `docs/` directory):

```shell
yarn dev
```

## Building

To produce the static site and preview it:

```shell
yarn build
yarn astro preview
```

Substitute the package manager of your choice for `yarn`.

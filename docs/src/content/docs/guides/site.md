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

## Cutting a release

The repository uses semantic version tags with a leading `v`, such as `v1.1.0`. The GitHub Actions workflow in `.github/workflows/release.yml` runs on a pushed matching tag and creates the GitHub Release from the corresponding section of `CHANGELOG.md`. It uses a GitHub-hosted Ubuntu runner; there is no self-hosted runner to maintain.

Before releasing:

1. Add a heading such as `## [1.1.0] - 2026-09-16` to `CHANGELOG.md`. The release action requires a matching entry.
2. Regenerate the Emacs documentation with `./src/scripts/org_to_md.sh`.
3. Build the site with `yarn --cwd docs build`.
4. Confirm `git diff --check` passes and `git status --short` contains only intended release files.
5. Commit and push the release branch, then merge it into `main`.
6. Tag the release commit and push the tag:

```shell
git switch main
git pull --ff-only origin main
git tag -a v1.1.0 -m "v1.1.0"
git push origin refs/tags/v1.1.0
```

Pushing the tag is the event that starts the release workflow. A branch named after a version does not trigger it, and `1.1.0v` is not a supported version tag. Using the fully qualified `refs/tags/...` form also avoids ambiguity if a branch and tag ever share a name.

After pushing, verify the `release` workflow in GitHub Actions and check that the generated GitHub Release contains the 1.1.0 changelog text. Do not move or reuse a published version tag; make a new patch version if another release is needed.

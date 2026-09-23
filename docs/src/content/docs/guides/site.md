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

Run the final checks from the repository root:

```shell
./src/scripts/org_to_md.sh
yarn --cwd docs build
git diff --check
git status --short
```

Stage only the files intended for the release. Avoid using `git add .` without reviewing the untracked files first, because this repository may contain machine-local application state.

```shell
git add <intended-release-files>
git diff --cached --stat
git status
git commit -m "release: prepare v1.1.0"
git push origin v1.1.0
```

Open a pull request from `v1.1.0` to `main` on GitHub and merge it. The release workflow requires the tagged commit to be on `main`, so do not tag the release branch before it has been merged. After the merge, update the local `main` branch and confirm that it contains the matching changelog entry:

```shell
git switch main
git pull --ff-only origin main
grep -nF '## [1.1.0]' CHANGELOG.md
```

Create an annotated tag on that `main` commit, inspect it, and push it explicitly as a tag:

```shell
git tag -a v1.1.0 -m "v1.1.0"
git show --no-patch v1.1.0
git push origin refs/tags/v1.1.0
```

Pushing the tag is the event that starts the release workflow. A branch named after a version does not trigger it, and `1.1.0v` is not a supported version tag. A branch and tag may both be named `v1.1.0`; using the fully qualified `refs/tags/...` form avoids ambiguity.

After pushing, verify the `release` workflow in GitHub Actions and check that the generated GitHub Release contains the 1.1.0 changelog text. If the GitHub CLI is installed, the same checks can be made from the terminal:

```shell
gh run list --workflow release.yml
gh release view v1.1.0
```

Do not move or reuse a published version tag; make a new patch version if another release is needed.

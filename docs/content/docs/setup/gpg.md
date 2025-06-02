---
weight: 1
bookFlatSection: true
title: "GPG Setup"
---

# Introduction


# Signing for Github

After creating the `gpg-key` make sure the follow the directions on `github` to upload the key on your account. After that is done follow the instructions below.

# Signing on Local Machine

Make sure the `.gitconfig` is configured to have the `signingkey` ready. To do so you can simply run:

```shell
  git config --global user.signingkey <YOUR_KEY_ID>
```

Trust the key (can skip, however may be necessary).

```shell
  gpg --edit-key <YOUR_KEY_ID>
```

Start the =gpg-agent=.

```shell
  gpgconf --launch gpg-agent
```

Use =pinentry=. Depending on your os, there are different versions of `pinentry`. For example for `macos` there is `pinentry-mac`.

For `pinentry` create a `~/.gnupg/gpg-agent.conf` file and add:

```shell
  # if not sure of path run `which pinentry` for the case of macos
  # run `which pinentry-mac`

  pinentry-program <PATH_TO_PINENTRY>
```

Restart the `gpg-agent`.

```shell
  gpgconf --kill gpg-agent
  gpgconf --launch gpg-agent
```

Make sure the `GPG_TTY` environment variable is set and then source the configuration.

```shell
  export GPG_TTY=$(tty)
```

---
title: The main initialization of Emacs (`init.el`)
description: Documentation for The main initialization of Emacs (`init.el`)
---


## The `init.el` key installations post-`early-init.el`

These installations do not require the package manager to be configured and are also essential for the functioning of the rest of this configuration.

### straight.el

```emacs-lisp

    ;; installation of straight.el package manager
    (defvar bootstrap-version)
    (let ((bootstrap-file
            (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))

```

## The `init.el` for aesthetics

Want to disable to mode-line at the very start and enable it when the mode-line configuration is loaded to make startup look smooth. The mode-line will be added when the mode-line is loaded ([The `jw-emacs-modeline.el` to enable the mode-line](*The `jw-emacs-modeline.el` to enable the mode-line)).

```emacs-lisp

    ;; conditional to remove display of mode-line
    (setq-default mode-line-format nil)

```


```emacs-lisp

    (require 'dired)

    ;; make dired look nice
    (add-hook 'dired-mode-hook
                (lambda ()
                (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
                ;; Uncomment the next line to start with details hidden
                (dired-hide-details-mode 1)
                ))

```


## The `init.el` for package manager

[use-package](https:/*github.com*jwiegley/use-package) is a native package built into emacs since `v29.0.0` and is used in this configuration to make it a lot easier to automate the installation and configuration of everything else.

```emacs-lisp

    ;; Install use-package
    (straight-use-package 'use-package)

    (setq straight-use-package-by-default t)

```

To debug `use-package` run `emacs --debug-init`.

```emacs-lisp

  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

```


## The `init.el` for keeping `.emacs.d` clean

```emacs-lisp

  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

  ;; auto-save-mode doesn't create the path automatically!
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

  (setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

  (use-package no-littering
    :straight t)

```

## The `init.el` for easy leader key management (`general.el`)

[general.el](https:/*github.com*noctuid/general.el) is a fantastic library for defining prefixed keybindings, especially
in conjunction with Evil modes.

```emacs-lisp

  (use-package general
    :straight t
    :config
    (general-create-definer jw/leader-key-def
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC"))

```

## The `init.el` essential key configurations

```emacs-lisp

  ;; bind quit prompting func to escape
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

```


```emacs-lisp

  ;; use spaces instead of tabs for indentation
  (setq-default indent-tabs-mode nil)

```


```emacs-lisp
  ;; for ui toggles
  (jw/leader-key-def
    "t"  '(:ignore t :which-key "toggles")
    "tw" 'whitespace-mode
    )

```

### evil-mode

```emacs-lisp
  (use-package evil
    :straight t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :config
    (evil-mode 1)
    (evil-set-undo-system 'undo-redo)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
    (define-key evil-normal-state-map (kbd "C-r") 'evil-redo)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  (use-package evil-collection
    :after evil
    :straight t
    :config
    (evil-collection-init)
    (with-eval-after-load 'forge
    (evil-collection-forge-setup)))


```

### smooth scrolling

```emacs-lisp

  ;; Enable smooth scrolling with a margin
  (setq scroll-margin 5)          ; Start scrolling when cursor is 5 lines from top/bottom
  (setq scroll-conservatively 100) ; Scroll line by line, not by half-screen jumps
  (setq scroll-step 1)            ; Scroll one line at a time when needed


```

## The `init.el` setting to always start with the **scratch** buffer

```emacs-lisp

  ;; Always start with *scratch*
  (setq initial-buffer-choice t)

```
## The `init.el` for additional configurations for `emacs`

```emacs-lisp

  ;; A few more useful configurations...
  (use-package emacs
    :straight t
    :init
    ;; TAB cycle if there are only few candidates
    ;; (setq completion-cycle-threshold 3)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    ;; (setq tab-always-indent 'complete)

    ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
    ;; try `cape-dict'.
    (setq text-mode-ispell-word-completion nil)

    ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
    ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
    ;; setting is useful beyond Corfu.
    (setq read-extended-command-predicate #'command-completion-default-include-p))

```

## The `init.el` user options

```emacs-lisp

  ;; taken from prot's config
  ;; For those who use my dotfiles and need an easy way to write their
  ;; own extras on top of what I already load: search below for the files
  ;; prot-emacs-pre-custom.el and prot-emacs-post-custom.el
  (defgroup jw-emacs nil
    "User options for my dotemacs.
  These produce the expected results only when set in a file called
  prot-emacs-pre-custom.el.  This file must be in the same
  directory as the init.el."
    :group 'file)

```
## The `init.el` user option to enable which-key

The `which-key` package provides hints for keys that complete the currently incomplete sequence. Here we determine whether to load the module or not. I personally never rely on `which-key` even if I enable its mode. If I ever need to review which key bindings are available I will either type `C-h` to complete a key sequence (produces a Help buffer with relevant keys) or I will do `C-h m` (`M-x describe-mode` to get information about the current major mode).

Remember to read how these options come into effect ([The init.el user options](https:/*protesilaos.com*emacs/dotemacs#h:5a41861f-4c38-45ac-8da2-51d77c0b4a73)).

Also check the [prot-emacs-which-key.el module](https:/*protesilaos.com*emacs/dotemacs#h:ddb1070d-2f91-4224-ad43-ef03f038f787).

```emacs-lisp

  (defcustom jw-emacs-load-which-key t
    "When non-nil, display key binding hints after a short delay.
  This user option must be set in the `prot-emacs-pre-custom.el'
  file.  If that file exists in the Emacs directory, it is loaded
  before all other modules of my setup."
    :group 'jw-emacs
    :type 'boolean)

```

## The `init.el` user option to load a theme family

```emacs-lisp


  (defcustom jw-emacs-load-theme-family 'modus
    "Set of themes to load.
  Valid values are the symbols `ef', `modus', and `standard', which
  reference the `ef-themes', `modus-themes', and `standard-themes',
  respectively.

  A nil value does not load any of the above (use Emacs without a
  theme).

  This user option must be set in the `jw-emacs-pre-custom.el'
  file.  If that file exists in the Emacs directory, it is loaded
  before all other modules of my setup."
    :group 'jw-emacs
    :type '(choice :tag "Set of themes to load" :value modus
                   (const :tag "The `ef-themes' module" ef)
                   (const :tag "The `modus-themes' module" modus)
                   (const :tag "The `standard-themes' module" standard)
                   (const :tag "Do not load a theme module" nil)))

```

## The `init.el` final part to load the individual modules

Load the `jw-emacs-modules`.

```emacs-lisp
  (load (locate-user-emacs-file "jw-emacs-pre-custom.el") :no-error :no-message)
  (require 'jw-emacs-theme)
  (require 'jw-emacs-essentials)
  (require 'jw-emacs-modeline)
  (require 'jw-emacs-completion)
  (require 'jw-emacs-org)
  (require 'jw-emacs-git)
  (require 'jw-emacs-dired)
  (require 'jw-emacs-information-management)
  (require 'jw-emacs-productivity)
  (require 'jw-emacs-development)
  (require 'jw-emacs-which-key)
  (require 'jw-emacs-ai)
  (require 'jw-emacs-langs)

```

Load `jw-lisp`.

```emacs-lisp

  (require 'jw-copy)

```
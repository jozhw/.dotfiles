---
title: The modules of my Emacs configuration (`jw-emacs-modules/`)
description: Documentation for The modules of my Emacs configuration (`jw-emacs-modules/`)
---

## The `jw-emacs-theme.el` module
### The `jw-emacs-theme.el` section to load a theme (`jw-emacs-load-theme-family`)

```emacs-lisp

  ;;; Theme setup and related

  ;;;; Load the desired theme module
  ;; These all reference my packages: `modus-themes', `ef-themes',
  ;; `standard-themes'.
  (when jw-emacs-load-theme-family
    (require
     (pcase jw-emacs-load-theme-family
       ('ef 'jw-emacs-ef-themes)
       ('modus 'jw-emacs-modus-themes)
       ('standard 'jw-emacs-standard-themes))))

```

#### The `jw-emacs-modus-themes.el` module -- native

```emacs-lisp

  ;;; The Modus themes

  ;; The themes are highly customisable.  Read the manual:
  ;; <https://protesilaos.com/emacs/modus-themes>.
  (use-package modus-themes
    :ensure t
    :demand t
    :bind (("<f5>" . modus-themes-toggle)
           ("C-<f5>" . modus-themes-select))
    :config
    (setq modus-themes-custom-auto-reload nil
          modus-themes-to-toggle '(modus-operandi modus-vivendi)
          ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
          ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
          ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui t
          modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
          modus-themes-completions '((t . (extrabold)))
          modus-themes-prompts '(extrabold)
          modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.15))))

    (setq modus-themes-common-palette-overrides nil))
    (if (jw-emacs-theme-environment-dark-p)
      (modus-themes-load-theme (cadr modus-themes-to-toggle))
      (modus-themes-load-theme (car modus-themes-to-toggle)))
   (provide 'jw-emacs-modus-themes)

```

#### The `jw-emacs-ef-themes.el` module

```emacs-lisp

  ;;; The Ef (εὖ) themes

  ;; The themes are customisable.  Read the manual:
  ;; <https://protesilaos.com/emacs/ef-themes>.
  (use-package ef-themes
    :ensure t
    :demand t
    :bind ("<f5>" . ef-themes-select)
    :config
    (setq ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t
          ef-themes-headings ; read the manual's entry of the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch regular 1.7))
            (3 . (variable-pitch regular 1.6))
            (4 . (variable-pitch regular 1.5))
            (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
            (6 . (variable-pitch 1.3))
            (7 . (variable-pitch 1.2))
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.9))
            (t . (variable-pitch 1.1))))

    ;; The `ef-themes' provide lots of themes.  I want to pick one at
    ;; random when I start Emacs: the `ef-themes-load-random' does just
    ;; that (it can be called interactively as well).  I just check with
    ;; my desktop environment to determine if the choice should be about
    ;; a light or a dark theme.  Those functions are in my init.el.
    (if (jw-emacs-theme-environment-dark-p)
        (ef-themes-load-random 'dark)
      (ef-themes-load-random 'light)))
(provide 'jw-emacs-ef-themes)
```
### The `jw-emacs-theme.el` section for `pulsar`

```emacs-lisp

  ;;;; Pulsar
  ;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
  (use-package pulsar
    :ensure t
    :config
    (setopt pulsar-pulse t
            pulsar-delay 0.055
            pulsar-iterations 10
            pulsar-face 'pulsar-magenta
            pulsar-highlight-face 'pulsar-cyan)

    (pulsar-global-mode 1)
    :hook
    ;; There are convenience functions/commands which pulse the line using
    ;; a specific colour: `pulsar-pulse-line-red' is one of them.
    ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
     (minibuffer-setup . pulsar-pulse-line-red))
    :bind
    ;; pulsar does not define any key bindings.  This is just my personal
    ;; preference.  Remember to read the manual on the matter.  Evaluate:
    ;;
    ;; (info "(elisp) Key Binding Conventions")
    (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
     ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'
```

### The `jw-emacs-theme.el` section for `lin`

```emacs-lisp

  ;;;; Lin
  ;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
  (use-package lin
    :ensure t
    :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
    :config
    ;; You can use this to live update the face:
    ;;
    ;; (customize-set-variable 'lin-face 'lin-green)
    ;;
    ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
    ;;
    ;; I still prefer `setq' for consistency.
    (setq lin-face 'lin-magenta))

```
### The `jw-emacs-theme.el` section for `spacious-padding`

```emacs-lisp

  ;;;; Increase padding of windows/frames
  ;; Yet another one of my packages:
  ;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
  (use-package spacious-padding
    :ensure t
    :if (display-graphic-p)
    :hook (after-init . spacious-padding-mode)
    :bind ("<f8>" . spacious-padding-mode)
    :init
    ;; These are the defaults, but I keep it here for visiibility.
    (setq spacious-padding-widths
          '( :internal-border-width 15
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 1
             :scroll-bar-width 8
             :left-fringe-width 20
             :right-fringe-width 20))

    ;; Read the doc string of `spacious-padding-subtle-mode-line' as
    ;; it is very flexible.
    (setq spacious-padding-subtle-mode-line
          `( :mode-line-active ,(if (or (eq jw-emacs-load-theme-family 'modus)
                                        (eq jw-emacs-load-theme-family 'standard))
                                    'default
                                  'help-key-binding)
             :mode-line-inactive window-divider)))

```
### The `jw-emacs-theme.el` section for `cursory`

```emacs-lisp

  ;;; Cursor appearance (cursory)
  ;; Read the manual: <https://protesilaos.com/emacs/cursory>.
  (use-package cursory
    :ensure t
    :demand t
    :if (display-graphic-p)
    :config
    (setq cursory-presets
          '((box
             :blink-cursor-interval 1.2)
            (box-no-blink
             :blink-cursor-mode -1)
            (bar
             :cursor-type (bar . 2)
             :blink-cursor-interval 0.8)
            (bar-no-other-window
             :inherit bar
             :cursor-in-non-selected-windows nil)
            (bar-no-blink
             :cursor-type (bar . 2)
             :blink-cursor-mode -1)
            (underscore
             :cursor-type (hbar . 3)
             :blink-cursor-blinks 50)
            (underscore-thin-other-window
             :inherit underscore
             :cursor-in-non-selected-windows (hbar . 1))
            (underscore-thick
             :cursor-type (hbar . 8)
             :blink-cursor-interval 0.3
             :blink-cursor-blinks 50
             :cursor-in-non-selected-windows (hbar . 3))
            (underscore-thick-no-blink
             :blink-cursor-mode -1
             :cursor-type (hbar . 8)
             :cursor-in-non-selected-windows (hbar . 3))
            (t ; the default values
             :cursor-type box
             :cursor-in-non-selected-windows hollow
             :blink-cursor-mode 1
             :blink-cursor-blinks 10
             :blink-cursor-interval 0.2
             :blink-cursor-delay 0.2)))

    ;; I am using the default values of `cursory-latest-state-file'.

    ;; Set last preset or fall back to desired style from `cursory-presets'.
    (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
    :hook
    ;; The other side of `cursory-restore-latest-preset'.
    (kill-emacs . cursory-store-latest-preset)
    :bind
    ;; We have to use the "point" mnemonic, because C-c c is often the
    ;; suggested binding for `org-capture' and is the one I use as well.
    ("C-c p" . cursory-set-preset))
```

### The `jw-emacs-theme.el` section for `theme-buffet`

```emacs-lisp

   ;;;; Theme buffet
  (use-package theme-buffet
    :ensure t
    :after (:any modus-themes ef-themes)
    :defer 1
    :config
    (let ((modus-themes-p (featurep 'modus-themes))
          (ef-themes-p (featurep 'ef-themes)))
      (setq theme-buffet-menu 'end-user)
      (setq theme-buffet-end-user
            (cond
             ((and modus-themes-p ef-themes-p)
              '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
                 :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
                 :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
                 :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
             (ef-themes-p
              '( :night     (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
                 :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
                 :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
                 :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
             (modus-themes-p
              '( :night     (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)
                 :morning   (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
                 :afternoon (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
                 :evening   (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)))))

      (when (or modus-themes-p ef-themes-p)
        (theme-buffet-timer-hours 1))))

```
### The `jw-emacs-theme.el` section for `fontaine`

```emacs-lisp

  ;;;; Fontaine (font configurations)
  ;; Read the manual: <https://protesilaos.com/emacs/fontaine>
  (use-package fontaine
    :ensure t
    :if (display-graphic-p)
    :hook
    ;; Persist the latest font preset when closing/starting Emacs and
    ;; while switching between themes.
    ((after-init . fontaine-mode)
     (after-init . (lambda ()
                          ;; Set last preset or fall back to desired style from `fontaine-presets'.
                          (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
    :bind ("C-c f" . fontaine-set-preset)
    :config
    ;; This is defined in Emacs C code: it belongs to font settings.
    (setq x-underline-at-descent-line nil)

    ;; And this is for Emacs28.
    (setq-default text-scale-remap-header-line t)

    ;; This is the default value.  Just including it here for
    ;; completeness.
    (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

    (setq fontaine-presets
          '((small
             :default-family "Iosevka Comfy Motion"
             :default-height 80
             :variable-pitch-family "Iosevka Comfy Duo")
            (regular) ; like this it uses all the fallback values and is named `regular'
            (medium
             :default-weight semilight
             :default-height 115
             :bold-weight extrabold)
            (large
             :inherit medium
             :default-height 150)
            (live-stream
             :default-family "Iosevka Comfy Wide Motion"
             :default-height 150
             :default-weight medium
             :fixed-pitch-family "Iosevka Comfy Wide Motion"
             :variable-pitch-family "Iosevka Comfy Wide Duo"
             :bold-weight extrabold)
            (presentation
             :default-height 180)
            (t
             ;; I keep all properties for didactic purposes, but most can be
             ;; omitted.  See the fontaine manual for the technicalities:
             ;; <https://protesilaos.com/emacs/fontaine>.
             :default-family "Iosevka Comfy"
             :default-weight regular
             :default-slant normal
             :default-height 180

             :fixed-pitch-family "Iosevka Comfy"
             :fixed-pitch-weight nil
             :fixed-pitch-slant nil
             :fixed-pitch-height 1.0

             :fixed-pitch-serif-family nil
             :fixed-pitch-serif-weight nil
             :fixed-pitch-serif-slant nil
             :fixed-pitch-serif-height 1.0

             :variable-pitch-family "Iosevka Comfy Motion Duo"
             :variable-pitch-weight nil
             :variable-pitch-slant nil
             :variable-pitch-height 1.0

             :mode-line-active-family nil
             :mode-line-active-weight nil
             :mode-line-active-slant nil
             :mode-line-active-height 1.0

             :mode-line-inactive-family nil
             :mode-line-inactive-weight nil
             :mode-line-inactive-slant nil
             :mode-line-inactive-height 1.0

             :header-line-family nil
             :header-line-weight nil
             :header-line-slant nil
             :header-line-height 1.0

             :line-number-family nil
             :line-number-weight nil
             :line-number-slant nil
             :line-number-height 1.0

             :tab-bar-family nil
             :tab-bar-weight nil
             :tab-bar-slant nil
             :tab-bar-height 1.0

             :tab-line-family nil
             :tab-line-weight nil
             :tab-line-slant nil
             :tab-line-height 1.0

             :bold-family nil
             :bold-weight bold
             :bold-slant nil
             :bold-height 1.0

             :italic-family nil
             :italic-weight nil
             :italic-slant italic
             :italic-height 1.0

             :line-spacing nil)))

    (with-eval-after-load 'pulsar
      (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))


```
### The `jw-emacs-theme.el` section for font resizing and `variable-pitch-mode` --native

```emacs-lisp

    ;;;;; `variable-pitch-mode' setup
  (use-package face-remap
    :ensure nil
    :functions jw/enable-variable-pitch
    :bind ( :map ctl-x-x-map
            ("v" . variable-pitch-mode))
    :hook ((text-mode notmuch-show-mode elfeed-show-mode) . jw/enable-variable-pitch)
    :config
    ;; NOTE 2022-11-20: This may not cover every case, though it works
    ;; fine in my workflow.  I am still undecided by EWW.
    (defun jw/enable-variable-pitch ()
      (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
        (variable-pitch-mode 1)))
  ;;;;; Resize keys with global effect
    :bind
    ;; Emacs 29 introduces commands that resize the font across all
    ;; buffers (including the minibuffer), which is what I want, as
    ;; opposed to doing it only in the current buffer.  The keys are the
    ;; same as the defaults.
    (("C-x C-=" . global-text-scale-adjust)
     ("C-x C-+" . global-text-scale-adjust)
     ("C-x C-0" . global-text-scale-adjust)))

```

### The `jw-emacs-theme.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-theme)

```

## The `jw-emacs-essentials.el` module
### The `jw-emacs-essentials.el` section for gui configurations

Set up the visible bell to be on instead of the beeping. For macos it is best to leave commented below to not have the visual bell because it is distracting.
```emacs-lisp

  ;; (setq visible-bell t)

```


Enable line numbers globally, but not in the following modes: org, term, shell, and eshell.

In addition to line numbers, the column number will also be displayed. 

```emacs-lisp

  ;; Enable column numbers
  (column-number-mode)

  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  markdown-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

```

Since `fill-paragraph` wraps `fill-column`, we adjust the size of the `fill-column` variable.

```emacs-lisp

  (setq-default fill-column 80)

```

### The `jw-emacs-essentials.el` section for window configurations

```emacs-lisp

  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down)
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)

```

### The `jw-emacs-essentials.el` section for `helpful.el`

[Helpful](https:/*github.com*Wilfred/helpful) adds a lot of very helpful (get it?) information to Emacs' `describe-` command buffers.  For example, if you use `describe-function`, you will not only get the documentation about the function, you will also see the source code of the function and where it gets used in other places in the Emacs configuration.  It is very useful for figuring out how things work in Emacs.

```emacs-lisp

  (use-package helpful
    :bind
    ([remap describe-command] . helpful-command)
    ([remap describe-key] . helpful-key))

```

### The `jw-emacs-essentials.el` for `auth-sources.el` -- native


```emacs-lisp

  (setq auth-sources '("~/.authinfo" "~/.netrc"))

```

### The `jw-emacs-essentials.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-essentials)

```

## The `jw-emacs-modeline.el` module
### The `jw-emacs-modeline.el` to enable the mode-line
The mode-line was disabled earlier ([The `init.el` conditional to remove display of mode-line](*The `init.el` conditional to remove display of mode-line)) so that the startup UI would look smooth

```emacs-lisp

  (setq-default mode-line-format (default-value 'mode-line-format))

```

### The `jw-emacs-modeline.el` basic user interface configuration

```emacs-lisp

  (setq display-time-format "%l:%M %p %b %y"
        display-time-default-load-average nil)

```

### The `jw-emacs-modeline.el` customization with `doom-modeline`

[doom-modeline](https:/*github.com*seagle0128*doom-modeline) is a very attractive and rich (yet still minimal) mode line configuration for Emacs.  The default configuration is quite good but you can check out the [configuration options](https:**github.com*seagle0128/doom-modeline#customize) for more things you can enable or disable.

If you are running in the `macos` terminal, then you have to make sure that you set the font to `Droid Sans Mono Nerd Font Complete 18`. You can do this by the following steps:

- Navigate to `Settings`
- Navigate to `Profiles` tab
- Navigate to `Text` subtab
- Under the `Font` menu click on `Change`
- Select the appropriate font


```emacs-lisp

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

```


`doom-modeline` icons rely on `nerd-icons`. Thus, you must install the `nerd-icons` if you want to use the icons on the modeline.

IMPORTANT: must run the following command — `Mx - nerd-icons-install-fonts` for the icons to populate. See the github issue here: [Doom Emacs Issue #7368](https:/*github.com*doomemacs*doomemacs*issues/7368#issuecomment-1689292109)

```emacs-lisp

  (use-package nerd-icons
    ;; :custom
    ;; The Nerd Font you want to use in GUI
    ;; "Symbols Nerd Font Mono" is the default and is recommended
    ;; but you can use any other Nerd Font if you want
    ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
    )

```


To turn off icons uncomment the following:

```emacs-lisp

  ;; (setq doom-modeline-icon nil)

```


The following contains configurations of the `doom-modeline`. All the configurations here use the `setq`.


```emacs-lisp

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

```


Display the virtual environment version.

```emacs-lisp

  (setq doom-modeline-env-version t)

```

### The `jw-emacs-modeline.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-modeline)

```

## The `jw-emacs-completion.el` module
### The `jw-emacs-completion.el` section for preserving minibuffer history (`savehist.el`)

```emacs-lisp

  (use-package savehist
    :config
    (setq history-length 25)
    (savehist-mode 1))

    ;; Individual history elements can be configured separately
    ;;(put 'minibuffer-history 'history-length 25)
    ;;(put 'evil-ex-history 'history-length 50)
    ;;(put 'kill-ring 'history-length 25))

```

### The `jw-emacs-completion.el` section for completions (`vertico.el`)

```emacs-lisp

  (defun jw/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
  folder, otherwise delete a word"
    (interactive "p")
    (if minibuffer-completing-file-name
        ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
        (if (string-match-p "/." (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
          (delete-minibuffer-contents))
        (delete-word (- arg))))

  (use-package vertico
    :ensure t
    :bind (:map vertico-map
           ("C-j" . vertico-next)
           ("C-k" . vertico-previous)
           ("C-f" . vertico-exit)
           :map minibuffer-local-map
           ("M-h" . jw/minibuffer-backward-kill))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

```

#### Troubleshooting

If in the Emacs buffer if you get a `Error in post-command-hook (vertico--exhibit): (void-function compat--completion-metadata-get)` error then you either delete vertico and recomplile or recompile

```

  M-x package-recompile
  vertico

```

The reason this issue is observed is due to updating Emacs.

A link to the github issue where I found the solution is [here](https:/*github.com*minad*vertico*discussions/501#discussioncomment-12390155).

### The `jw-emacs-completion.el` section for completions in region (`corfu.el`)

```emacs-lisp
  (use-package corfu
    :ensure t
    ;; Optional customizations
    :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.8)
    (corfu-popinfo-delay '(0.5 . 0.2))
    (corfu-preview-current 'insert) ; insert previewed candidate
    (corfu-preselect 'prompt)
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin
    :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("C-f" . corfu-insert))
    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
    ;; be used globally (M-/).  See also the customization variable
    ;; `global-corfu-modes' to exclude certain modes.
    :init
    (global-corfu-mode))

```
### The `jw-emacs-completion.el` section for completions in region on terminal

Since `corfu.el` does not support running emacs in the terminal, I will just stick with `company.el` instead of `corfu-terminal`.

[Company Mode](http:/*company-mode.github.io*) provides a nicer in-buffer completion interface than `completion-at-point` which is more reminiscent of what you would expect from an IDE.  We add a simple configuration to make the keybindings a little more useful (`TAB` now completes the selection and initiates completion at the current location if needed).

We also use [company-box](https:/*github.com*sebastiencs/company-box) to further enhance the look of the completions with icons and better overall presentation.


```emacs-lisp

  (unless (display-graphic-p)
      (progn
        ;; Configuration for GUI mode
        (use-package company
          :after eglot
          :hook (eglot--managed-mode . company-mode)
          :bind (:map company-active-map
                 ("<tab>" . company-complete-selection))
                (:map eglot-mode-map
                 ("<tab>" . company-indent-or-complete-common))
          :custom
          (company-minimum-prefix-length 1)
          (company-idle-delay 0.0))
      
        (use-package company-box
          :hook (company-mode . company-box-mode)))
    ;; Configuration for terminal mode (optional)
    ;; Add your terminal mode specific configuration here
    )

```

### The `jw-emacs-completion.el` section for additional completions in region (`cape.el`)

```emacs-lisp

  (use-package cape
    :ensure t
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

```

### The `jw-emacs-completion.el` section for candidate filtering (`orderless.el`)

```emacs-lisp

  (use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))

```

### The `jw-emacs-completion.el` section for completion annotations (`marginalia.el`)

Marginalia provides helpful annotations for various types of minibuffer completions. You can think of it as a replacement of `ivy-rich`.

```emacs-lisp

  (use-package marginalia
    :after vertico
    :ensure t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

```

### The `jw-emacs-completion.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-completion)

```

## The `jw-emacs-org.el` module
:PROPERTIES:
:ID:       77434357-7761-4049-9F64-1808E10E549D
:END:
### The `jw-emacs-org.el` section for org-mode setup --native

Set up Org Mode with a baseline configuration. The following sections will add more things to it.

```emacs-lisp

  (defun jw/org-mode-setup ()
    (org-indent-mode) ;; auto-indentation for headings
    (variable-pitch-mode 1) ;; cause fonts to vary by proportionality
    (visual-line-mode 1)) ;; wrap the text so that it does not go out of view

  (use-package org
    :hook (org-mode . jw/org-mode-setup)
    :config
    (setq org-ellipsis " ▾") ;; when org headings closed down arrow instead of ellipsis
    (setq org-M-RET-may-split-line '((default . nil))) ;; when auto generating subsequent headings, avoid splitting the line
    (setq org-insert-heading-respect-content t) ;; when creating new heading respects the content of which heading it was originally
    (setq org-log-done 'time)
    (setq org-log-into-drawer t) ;; task change is in drawer instead of content
    ;; keywords for org task states

    )


```

### the `jw-emacs-org.el` section for `org-agenda` --native
:PROPERTIES:
:ID:       43652950-B9D2-4FAF-8F0C-75D1496E85FE
:END:

```emacs-lisp

  ;; setting dir of tasks
  (setq org-agenda-files (directory-files-recursively "~/Otzar/Docs/agenda/" "\\.org$"))
  (setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))) 

```

### The `jw-emacs-org.el` section for org pomodoro timer --native

Configure for macos to play sound:

```emacs-lisp

  ;; on macos, fix "This Emacs binary lacks sound support" 
  ;; - https://github.com/leoliu/play-sound-osx/blob/master/play-sound.el
  ;; - update according to https://github.com/leoliu/play-sound-osx/issues/2#issuecomment-1088360638
  (when (eq system-type 'darwin)
    (unless (and (fboundp 'play-sound-internal)
                 (subrp (symbol-function 'play-sound-internal)))
      (defun play-sound-internal (sound)
        "Internal function for `play-sound' (which see)."
        (or (eq (car-safe sound) 'sound)
            (signal 'wrong-type-argument (list sound)))
 
        (cl-destructuring-bind (&key file data volume device)
            (cdr sound)
 
          (and (or data device)
               (error "DATA and DEVICE arg not supported"))
 
          (apply #'start-process "afplay" nil
                 "afplay" (append (and volume (list "-v" volume))
                                  (list (expand-file-name file data-directory))))))))


```


```emacs-lisp

  (setq org-clock-sound "~/.dotfiles/.assets/sounds/mixkit-alert-quick-chime-766.wav")

```

#### Sound Support

Usually this is a problem for macos and I found a snippet of code that enables sound support. The way to tell is by running `M-x play-sound-file` and navigating to the `.wav` file will ouput "This Emacs binary lacks sound support."

```emacs-lisp
  ;; on macos, fix "This Emacs binary lacks sound support" 
  ;; - https://github.com/leoliu/play-sound-osx/blob/master/play-sound.el
  ;; - update according to https://github.com/leoliu/play-sound-osx/issues/2#issuecomment-1088360638
  (when (eq system-type 'darwin)
    (unless (and (fboundp 'play-sound-internal)
                 (subrp (symbol-function 'play-sound-internal)))
      (defun play-sound-internal (sound)
        "Internal function for `play-sound' (which see)."
        (or (eq (car-safe sound) 'sound)
            (signal 'wrong-type-argument (list sound)))
      
        (cl-destructuring-bind (&key file data volume device)
            (cdr sound)
        
          (and (or data device)
               (error "DATA and DEVICE arg not supported"))
        
          (apply #'start-process "afplay" nil
                 "afplay" (append (and volume (list "-v" volume))
                                  (list (expand-file-name file data-directory))))))))

```

### The `jw-emacs-org.el` section for org-links

As recommended by the official `org` manual to have these keys bound.

```emacs-lisp

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c C-l") 'org-insert-link)

```

Instead of relying on just the path/etc, allow orgmode to use unique IDs to create internal links that can point to specific headings in org files.

With the `create-if-interactive` setting, it only creates in interactive settings.

```emacs-lisp

  (setq org-id-link-to-org-use-id 'create-if-interactive)

```

### The `jw-emacs-org.el` section for bullets

Customize the heading bullets to make it consistent and nicer.

```emacs-lisp

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

```

### The `jw-emacs-org.el` section for inline images


```emacs-lisp

  (setq org-image-actual-width nil)
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook 'org-display-inline-images)

```

### The `jw-emacs-org.el` section for `org-transclusion`

Link to the docs: [org-transclude](https:/*nobiot.github.io*org-transclusion/).


```emacs-lisp

  (unless (package-installed-p 'org-transclusion)
    (package-refresh-contents)
    (package-install 'org-transclusion))

  (require 'org-transclusion)

```

### The `jw-emacs-org.el` section for structured templates (`org-tempo`) --native

These structured templates are used to auto generate code blocks for org mode. In order
to use the template simply type `<` followed by the abbreviation of the language and
hit the `TAB` button. For example, the python snippit would be `<py TAB`.

```emacs-lisp

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("clang" . "src c"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))

```

### The `jw-emacs-org.el` section for auto-tangle

This snippet adds a hook to `org-mode` buffers so that `jw/org-babel-tangle-config` gets executed each time such a buffer gets saved.  This function checks to see if the file being saved is the Emacs.org file you're looking at right now, and if so, automatically exports the configuration here to the associated output files.

```emacs-lisp

  ;; Automatically tangle our Emacs.org config file when we save it
  (defun jw/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.dotfiles/Emacs.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jw/org-babel-tangle-config)))

```

### The `jw-emacs-org.el` section for executing code (`org-babel`)

To execute or export code in `org-mode` code blocks, you'll need to set up `org-babel-load-languages` for each language you'd like to use. [This page](https:/*orgmode.org*worg*org-contrib*babel/languages.html) documents all of the languages that you can use with `org-babel`.

```emacs-lisp

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

```

### The `jw-emacs-org.el` section for text display (`visual-fill-column.el`)

`visual-fill-column` will create a document looking display with the extra padding on the left and on the right.

```emacs-lisp

  (defun jw/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . jw/org-mode-visual-fill)
    (markdown-mode . jw/org-mode-visual-fill))

```
### The `jw-emacs-org.el` section for org-latex

```emacs-lisp

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("org-plain-no-section-numbering-latex"
                 "\\documentclass{article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
                 ("\\section*{%s}" . "\\section*{%s}")
                 ("\\subsection*{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph*{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph*{%s}" . "\\subparagraph*{%s}"))))

```

### The `jw-emacs-org.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-org)

```

## The `jw-emacs-git.el` module
### The `jw-emacs-git.el` section for `magit.el`

[Magit](https:/*magit.vc*) is the best Git interface I've ever used.  Common Git operations are easy to execute quickly using Magit's command panel system.

```emacs-lisp

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  (setq forge-add-default-bindings nil)

  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  ;; NOTE: Make sure to configure a GitHub token before using this package!
  ;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
  ;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
  (use-package forge
    :after magit
    )
  (use-package evil-collection
    :after (evil forge)
    :config
    (evil-collection-init)
    (evil-collection-forge-setup))

```

#### GPG Signing

When on the commit buffer, the argument for `gpg-signing` or `-S` may not be displayed. To resolve this issue manually, on the commit buffer menu, you must enter transient mode with `C-x l` and follow the prompting from there by typing the argument that you want to change the layering and then set the layering.

### The `jw-emacs-git.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-git)

```

## The `jw-emacs-dired.el` module
### The `jw-emacs-dired.el` section for dired display

```emacs-lisp

  (setq dired-listing-switches "-alD")

```

### The `jw-emacs-dired.el` section for gnu `gls`

For macos, make sure to have `coreutils` installed. To install run, `brew install coreutils`

```emacs-lisp

  (setq insert-directory-program "gls" 
       dired-use-ls-dired t)

```

### The `jw-emacs-dired.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-dired)

```

## The `jw-emacs-information-management.el` module
### The `jw-emacs-information-management.el` section for `denote`

```emacs-lisp

  (use-package denote
                   :ensure t)

  (setq denote-directory (expand-file-name "~/Otzar/Gnosis/"))
  (setq denote-save-buffer-after-creation nil)

```


Enable the denote dired mode for all files so that the components can easily be seen.

```emacs-lisp

  (add-hook 'dired-mode-hook #'denote-dired-mode)

```

```emacs-lisp

  (setq denote-known-keywords '("theology" "philosophy" "politics" "journal" "analysis" "linguistics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)

```

```emacs-lisp

  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(subdirectory title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-no-confirm nil) ; Set to t if you are familiar with `denote-rename-file'

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)
  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.
  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-fontify-links-mode-maybe)

  (with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well.


  ;; If you want to have Denote commands available via a right click
  ;; context menu, use the following and then enable
  ;; `context-menu-mode'.
  (add-hook 'context-menu-functions #'denote-context-menu)

```

Create [the `jw-emacs-org.el` section for `org-agenda` --native](id:43652950-B9D2-4FAF-8F0C-75D1496E85FE) org capture template leveraging the denote format.

```emacs-lisp

  ;; Ensure denote.el is loaded
  (require 'denote)

  (defun jw-denote-weekly-tasks-filename ()
  "Generate a Denote filename for a weekly tasks Org file in a custom directory and ensure the file exists.
  The title is in the format 'YYYY: MONTH DD to DD', where DD to DD represents
  the start and end days of the current week. The filename follows the Denote
  convention with the '__tasks' tag."
  (let* ((custom-directory "~/Otzar/Docs/agenda/")  ; Specify your custom directory here
          (today (current-time))
          ;; Calculate the start of the week (assuming Monday as the first day)
          (start-of-week (time-subtract today (days-to-time (mod (nth 6 (decode-time today)) 7))))
          ;; Calculate the end of the week (Sunday)
          (end-of-week (time-add start-of-week (days-to-time 6)))
          ;; Format the year and month from the start of the week
          (year (format-time-string "%Y" start-of-week))
          (month (format-time-string "%B" start-of-week))
          (day-start (format-time-string "%d" start-of-week))
          (day-end (format-time-string "%d" end-of-week))
          ;; Create the title in the format "YYYY MONTH DD to DD"
          (title (format "%s: %s %s to %s" year month day-start day-end))
          ;; Generate the slug for the title
          (slug (denote-sluggify-title title))
          ;; Generate the timestamp for the Denote filename
          (timestamp (format-time-string "%Y%m%dT%H%M%S" start-of-week))
          ;; Construct the full filename with Denote convention
          (filename (format "%s--%s__tasks.org" timestamp slug)))
      ;; Ensure the custom directory exists
      (make-directory custom-directory t)
      ;; Generate the full file path
      (let ((full-path (expand-file-name filename custom-directory)))
      ;; Create an empty file with Denote metadata if it doesn't exist
      (unless (file-exists-p full-path)
          (with-temp-buffer
            (insert (format "#+title:      %s\n#+date:       %s\n#+filetags:   :tasks:\n#+identifier: %s\n\n"
                          title
                          (format-time-string "[%Y-%m-%d %a %H:%M]" today)
                          timestamp))
          (write-file full-path)))
      full-path)))

  ;; Define the Org capture template
  (setq org-capture-templates
      '(("w" "Weekly Tasks" entry
          (file jw-denote-weekly-tasks-filename)
          ""
          :empty-lines 1
          )))


```

### The `jw-emacs-information-management.el` section for `ledger-mode`

```emacs-lisp

  (use-package ledger-mode
    :ensure t
    :mode (
           "\\.ledger\\'")
    :custom (ledger-clear-whole-transactions t))


```

### The `jw-emacs-information-management.el` section for clean directories

Move the `#<FILE>#` to a temporary directory instead of root directory.

```emacs-lisp

  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

  (setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

```

### The `jw-emacs-information-management.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-information-management)

```

## The `jw-emacs-productivity.el` module

### The `jw-emacs-productivity.el` section for `pdf-tools`

Make sure to run `M-x pdf-tools-install` after installation.

```emacs-lisp

  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    :hook (pdf-view-mode . (lambda () 
                         (display-line-numbers-mode -1)
                         (message "PDF Tools activated for this buffer"))))


```

#### Troubleshooting

##### 2025-01-15: Works

The issue with the ***2025-01-14*** is that if the installation works within the command line, when opening up a pdf file on Emacs would lead to the epdfserver crashing. This issue I found had to do with confict with `macports` being installed. If you uninstall macports, then the issue is resolved. 

##### 2025-01-14: Does Not Work

If you receive the option to rebuild the `epdfserver` and you agree to building on Emacs, there are instances where the build fails. When running `M-x pdf-tools-install` you will rebuild within Emacs and will obtain more information. If the error consists of not being able to find poppler, copy and paste the command used to run the installation and run it in the command line outside of emacs.


### The `jw-emacs-productivity.el` section for `org-noter` and `org-pdftools`

```emacs-lisp


  ;; Ensure org-noter is installed
  (use-package org-noter
    :ensure t
    :after (org pdf-tools)
    :config
    (setq org-noter-always-create-frame nil))

  ;; Ensure org-pdftools is set up to work with org-mode
  (use-package org-pdftools
    :ensure t
    :hook (org-mode . org-pdftools-setup-link))

  ;; Configure org-noter-pdftools
  (use-package org-noter-pdftools
    :after (org-noter pdf-tools)
    :config
    ;; Add a function to ensure precise note is inserted
    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
      (interactive "P")
      (org-noter--with-valid-session
       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                     (not org-noter-insert-note-no-questions)
                                                   org-noter-insert-note-no-questions))
             (org-pdftools-use-isearch-link t)
             (org-pdftools-use-freepointer-annot t))
         (org-noter-insert-note (org-noter--get-precise-info)))))

    ;; Fix for the specific issue
    (defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
    With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
             (ast (org-noter--parse-root))
             (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (org-with-wide-buffer
            (goto-char (org-element-property :begin ast))
            (if arg
                (org-entry-delete nil org-noter-property-note-location)
              (org-entry-put nil org-noter-property-note-location
                             (org-noter--pretty-print-location location))))))))

    ;; Add a hook for pdf-annot
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))

    ;; If you are working with EPUB files
    (use-package nov
      :ensure t)

    ;; If you are working with DJVU files
    (use-package djvu
      :ensure t))

```

### The `jw-emacs-productivity.el` section for `pdf-viewer` hooks

```emacs-lisp

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;   (defun jw-pdf-view-mode-hook ()                     ;;
  ;;   "Hook to run when entering pdf-view-mode."          ;;
  ;;   (display-line-numbers-mode -1))                     ;;
  ;;                                                       ;;
  ;; (add-hook 'pdf-view-mode-hook 'jw-pdf-view-mode-hook) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

```

### The `jw-emacs-productivity.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-productivity)

```


## The `jw-emacs-development.el` module
### The `jw-emacs-development.el` section for `tramp.el` --native

```emacs-lisp

    (use-package tramp
      :ensure t)
    (setq tramp-default-method "ssh")
    ;; for debugging
    (setq tramp-verbose 3)
    ;; some basic performance enhancements
    (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

```


#### Using Direct Async

Guide by [this blog post](https:/*coredumped.dev*2025*06*18*making-tramp-go-brrrr.*). 

```emacs-lisp

  (connection-local-set-profile-variables
  'remote-direct-async-process
  '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
  '(:application tramp :protocol "scp")
  'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)


```

#### Fixing Remote Compile

`compile` command disables remote ssh connection sharing, which will require you to reenter your password each time you connect. Want to enable this for convienence.

```emacs-lisp

  (with-eval-after-load 'tramp
  (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

```

#### Caching

Cache passwords until the end of the emacs session, which is default.

```emacs-lisp

  (setq password-cache-expiry nil)

```

```emacs-lisp

    ;; Configure TRAMP to use ~/.emacs.d/tmp/ for caching
    (let ((tramp-tmp-dir (expand-file-name "tmp/" user-emacs-directory)))
    ;; Ensure the directory exists
    (unless (file-directory-p tramp-tmp-dir)
        (make-directory tramp-tmp-dir t))

    ;; Set TRAMP cache directory
    (setq tramp-persistency-file-name (expand-file-name "tramp-cache" tramp-tmp-dir))

    ;; Set auto-save directory for remote files
    (setq tramp-auto-save-directory tramp-tmp-dir)

    ;; Optional: Set backup directory for remote files to tmp as well
    (setq tramp-backup-directory-alist `(("." . ,tramp-tmp-dir))))

    ;; Enable persistent caching
    (setq tramp-cache-read-persistent-data t)
    (setq tramp-cache-compress t)
    (setq remote-file-name-inhibit-cache nil)

    (defun memoize-remote (key cache orig-fn &rest args)
    "Memoize a value if the key is a remote path."
    (if (and key
            (file-remote-p key))
        (if-let ((current (assoc key (symbol-value cache))))
            (cdr current)
            (let ((current (apply orig-fn args)))
            (set cache (cons (cons key current) (symbol-value cache)))
            current))
        (apply orig-fn args)))

```


```emacs-lisp

    ;; Memoize current project
    (defvar project-current-cache nil)
    (defun memoize-project-current (orig &optional prompt directory)
    (memoize-remote (or directory
                        project-current-directory-override
                        default-directory)
                    'project-current-cache orig prompt directory))
    (advice-add 'project-current :around #'memoize-project-current)

    ;; Memoize magit top level
    (defvar magit-toplevel-cache nil)
    (defun memoize-magit-toplevel (orig &optional directory)
    (memoize-remote (or directory default-directory)
                    'magit-toplevel-cache orig directory))
    (advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

    ;; memoize vc-git-root
    (defvar vc-git-root-cache nil)
    (defun memoize-vc-git-root (orig file)
    (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
        ;; sometimes vc-git-root returns nil even when there is a root there
        (when (null (cdr (car vc-git-root-cache)))
        (setq vc-git-root-cache (cdr vc-git-root-cache)))
        value))
    (advice-add 'vc-git-root :around #'memoize-vc-git-root)

    ;; memoize all git candidates in the current project
    (defvar $counsel-git-cands-cache nil)
    (defun $memoize-counsel-git-cands (orig dir)
    ($memoize-remote (magit-toplevel dir) '$counsel-git-cands-cache orig dir))
    (advice-add 'counsel-git-cands :around #'$memoize-counsel-git-cands)

    ;; Optional: Function to clear cache when needed
    (defun jw/clear-tramp-cache ()
    "Clear TRAMP cache files in ~/.emacs.d/tmp/"
    (interactive)
    (let ((cache-file tramp-persistency-file-name))
        (when (file-exists-p cache-file)
        (delete-file cache-file)
        (message "TRAMP cache cleared"))))

```

### The `jw-emacs-development.el` section for `project.el`  --native

```emacs-lisp

  (require 'project)

```


Set the project paths, but currently the code below only works for `emacs 30+`.

```emacs-lisp
   (defun jw/project-prompter ()
        (read-file-name "Select a project folder:"
                        "~/Otzar/Projects/Code/"
                        nil
                        nil
                        nil
                        #'file-directory-p))
   (setq project-prompter #'jw/project-prompter)

```

### The `jw-emacs-development.el` section for visualizing parent delimiters --native

`show-paren-mode` allows one to see matching pairs of parentheses and other characters. When point is on the opening character of one of the paired characters, the other is highlighted. When the point is after the closing character of one of the paired characters, the other is highlighted. 

```emacs-lisp

  (show-paren-mode 1)

```

### The `jw-emacs-development.el` section for visualizing all delimiters

[rainbow-delimiters](https:/*github.com*Fanael/rainbow-delimiters) is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth.  This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.

```emacs-lisp

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

```

### The `jw-emacs-development.el` section for pairing delimiters (`electric-pair-mode`) --native

`electric-pair-mode` will auto pair delimiters for you. One issue with the auto pairing is the `<` character in `org-mode`. The following hook to the enabling of `electric-pair-mode` aims to solve the issue when in `org-mode`.

```emacs-lisp

  (electric-pair-mode t)

  (add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

```

### The `jw-emacs-development.el` section for pairing delimiters (`evil-surround`)

`evil-surround` emulates [surround.vim](https:/*github.com*tpope*vim-surround). For usage instructions visit [evil-surround](https:**github.com*emacs-evil/evil-surround)

```emacs-lisp

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

```

### The `jw-emacs-development.el` section for logging keys (`command-log-mode`)

[command-log-mode](https:/*github.com*lewang/command-log-mode) is useful for displaying a panel showing each key binding you use in a panel on the right side of the frame. Great for live streams and screencasts!


```emacs-lisp

  (use-package command-log-mode)

```

To activate `command-log-mode` you must first run `M-x global-command-log-mode` to have `command-log-mode` in every buffer and then run `M-x clm/toggle-command-log-buffer` to have the buffer be displayed.
### The `jw-emacs-development.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-development)

```

## The `jw-emacs-which-key.el` module

There is a user option in my setup to load this module ([The init.el option to enable which-key](https:/*protesilaos.com*emacs/dotemacs#h:24324854-1f8c-4d8b-aa7c-291de968cbf4)).

When the `which-key-mode` is enabled, any incomplete key sequence will produce a popup at the lower part of the Emacs frame showing keys that complete the current sequence together with the name of the command they are invoking.

```emacs-lisp

  (use-package which-key
    :ensure t
    :hook (after-init . which-key-mode)
    :config
    (setq which-key-separator "  ")
    (setq which-key-prefix-prefix "... ")
    (setq which-key-max-display-columns 3)
    (setq which-key-idle-delay 1.5)
    (setq which-key-idle-secondary-delay 0.25)
    (setq which-key-add-column-padding 1)
    (setq which-key-max-description-length 40))

  (provide 'jw-emacs-which-key)

```
## The `jw-emacs-ai.el` module

The purpose of this module is to have my integrations with llms or other ai models.


### The `jw-emacs-ai.el` section for `gptel`

Incorporates the use of llms in the emacs client. For a great summary of the features please see [Ben Simon's video](https:/*www.blogbyben.com*2024*08*gptel-mindblowing-integration-between.html). For accessing the source code please see [karthink's repo](https:/*github.com*karthink/gptel).

```emacs-lisp

  (use-package gptel
    :ensure t
    :after auth-source
    :init
    ;; Ensure auth-source is configured to find ~/.authinfo or ~/.authinfo.gpg
    (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
    :config
    ;; Helper function to read file contents
    (defun gptel-read-file (file-path)
        "Read the contents of FILE-PATH into a string, trimming whitespace."
        (if (and (file-exists-p file-path) (file-readable-p file-path))
            (with-temp-buffer
            (insert-file-contents file-path)
            (string-trim (buffer-string)))
        (progn
            (message "Warning: File %s is not readable or does not exist" file-path)
            "You are a polymath who is a helpful assistant. Respond concisely and accurately.")))

    ;; Define directives with file paths
    (setq gptel-directives
            (list
            (cons 'default "You are a polymath who is a helpful assistant. Respond concisely and accurately.")
            (cons 'coding (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/coding.txt"))
            (cons 'writing (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/writing.txt"))
            (cons 'research (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/research.txt"))))

    ;; Set default directive
    (setq gptel-default-directive 'coding)

    
    ;; Refresh directives dynamically
    (defun gptel-refresh-directives ()
        "Refresh gptel-directives by re-reading files."
        (interactive)
        (setq gptel-directives
            (list
            (cons 'default "You are a polymath who is a helpful assistant. Respond concisely and accurately.")
            (cons 'coding (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/coding.txt"))
            (cons 'writing (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/writing.txt"))
            (cons 'research (gptel-read-file "~/.dotfiles/.assets/gpt-prompts/research.txt"))))
        (message "Refreshed gptel-directives from files."))

    ;; Set OpenAI API key using gptel-api-key-from-auth-source
    (setq gptel-api-key (lambda ()
                          (auth-source-forget-all-cached)
                          (gptel-api-key-from-auth-source)))
    ;; Configure Google (Gemini) backend
    (gptel-make-gemini "Gemini" :stream t
                        :key (lambda ()
             (auth-source-forget-all-cached)
             (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")))
    ;; Configure Anthropic (Claude) backend
    (gptel-make-anthropic "Claude"
      :stream t
      :key (lambda ()
             (auth-source-forget-all-cached)
             (gptel-api-key-from-auth-source "console.anthropic.com")))
    ;; Optional: Enable debugging for auth-source issues
    ;; (setq auth-source-debug t)
  )


```

```emacs-lisp

    (defun gptel-save-response ()
    "Save the entire gptel buffer to a file with a user-provided name."
    (interactive)
    (unless (bound-and-true-p gptel-mode)
        (user-error "This command must be run in a gptel-mode buffer"))
    (let* ((response (buffer-string))
            (user-input (read-string "Enter a concise (2-5 words) filename description: ")))
        (if (string-empty-p response)
            (message "Error: Buffer is empty, cannot save file")
        (let* ((clean-name (if (and user-input (stringp user-input) (not (string-empty-p user-input)))
                                (string-trim (replace-regexp-in-string "[^a-zA-Z0-9-]" "" (replace-regexp-in-string "\\s+" "-" user-input)))
                            "fallback-name"))
                (timestamp (format-time-string "%Y%m%dT%H%M%S"))
                (base-dir "~/Otzar/llm-outputs/")
                (filename (concat (file-name-as-directory (expand-file-name base-dir)) timestamp "--" clean-name ".md")))
            (condition-case err
                (progn
                (make-directory base-dir t)
                (write-region (point-min) (point-max) filename nil 'silent)
                (message "Saved buffer to %s" filename))
            (error
            (message "Error saving file: %s" err)))))))


```


### The `jw-emacs-ai.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-ai)

```

## The `jw-emacs-langs.el` module

### The `jw-emacs-langs.el` section for `treesit` --native

Set language sources for treesit

```emacs-lisp

  (setq treesit-language-source-alist
        '((typescript .        ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (r "https://github.com/r-lib/tree-sitter-r")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          ))

  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source))
      (treesit-install-language-grammar (car source))))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
  (add-to-list 'major-mode-remap-alist '(
                                         (python-mode . python-ts-mode)
                                         (json-mode . json-ts-mode)
                                         (css-mode . css-ts-mode)
                                         (bash-mode . bash-ts-mode)
                                         (yaml-mode . yaml-ts-mode)
                                         (c++-mode . c++-ts-mode)
                                         (c-mode . c-ts-mode)
                                         ))
```


### The `jw-emacs-langs.el` section for `treesit-auto`

The issue with the built in `treesit.el` is that it does not auto default to which language server. In addition if you need to install you will have to input the url yourself. This package is here to automate the process.

```emacs-lisp

  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

```

Auto install grammars when missing

```emacs-lisp

  (setq treesit-auto-install 'prompt)

```


### The `jw-emacs-langs.el` section for `python` support

Configuring pythone envs with `conda`.

```emacs-lisp

  (unless (package-installed-p 'conda)
    (package-refresh-contents)
    (package-install 'conda))

  (require 'conda)
  (setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/"))
   (setq conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/envs/"))
    (conda-env-autoactivate-mode t)


```

Define `jw/find-python-langserver`

```emacs-lisp

  (defun jw/find-pyright-langserver ()
    (or (executable-find "pyright-langserver")
        (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/bin/pyright-langserver")
        ))

```

```emacs-lisp

  (defun jw/local-pyright-command ()
    "Return the command to run the local Pyright server."
    (let ((pyright-path (jw/find-pyright-langserver)))
      (if pyright-path
          (list pyright-path "--stdio")
        (error "Could not find pyright-langserver"))))

```

Python formatter configuration.

```emacs-lisp

  (use-package python-black
    :demand t
    :after python
    :hook (python-ts-mode . python-black-on-save-mode))

```


### The `jw-emacs-langs.el` section for `astro` support

I used the [following guide](https:/*medium.com*@jrmjrm/configuring-emacs-and-eglot-to-work-with-astro-language-server-9408eb709ab0) from medium for this configuration.

```emacs-lisp

    ;; WEB MODE
    (use-package web-mode
    :ensure t)

    ;; astro
    ;; ASTRO
    (define-derived-mode astro-mode web-mode "astro")
    (setq auto-mode-alist
        (append '((".*\\.astro\\'" . astro-mode))
                auto-mode-alist))
  

```

Now set the config in [eglot](id:F9D087EE-895F-4DBC-BBCF-3056A2A5266E).  


### The `jw-emacs-langs.el` section for `rust` support

Download `rust-mode`.

```emacs-lisp

  (unless (package-installed-p 'rust-mode)
    (package-refresh-contents)
    (package-install 'rust-mode))

```

Setting up `rust-mode`.

```emacs-lisp

  (require 'rust-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

```

Custom function to find rust analyzer.

```emacs-lisp

  (defun jw/find-rust-analyzer ()
    (or (executable-find "rust-analyzer")
        (expand-file-name "~/.cargo/bin/rust-analyzer")))

```


### The `jw-emacs-langs.el` section for `typescript` support

Typescript support is done through `treesit.el`, which is now native to emacs as of v29.

Therefore all of the typescript will be using the tree-sitter equivalent.

```emacs-lisp

  ;; (use-package typescript-mode
  ;; :ensure t
  ;; :mode "\\.ts\\'")

```

Install `json-mode`

```emacs-lisp

  ;; (use-package json-mode
  ;; :ensure t
  ;; :mode "\\.json\\'")

```


### The `jw-emacs-langs.el` section for automating code formatting with `apheleia`

Make sure you have the necessary packages installed.

```emacs-lisp

  (use-package apheleia
    :ensure t
    :config
    (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
    ;; Map json-ts-mode to the prettier-json formatter
    (setf (alist-get 'json-ts-mode apheleia-mode-alist)
        '(prettier-json))
    (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(c++-ts-mode . clang-format))
    (add-to-list 'apheleia-mode-alist '(c-ts-mode . clang-format))
    (apheleia-global-mode +1))

```


### The `jw-emacs-langs.el` section for language server configuration (`eglot`) --native
:PROPERTIES:
:ID:       F9D087EE-895F-4DBC-BBCF-3056A2A5266E
:END:


```emacs-lisp

    ;; Dynamic server program functions
    (defun jw/python-lsp-program (&optional interactive)
    "Get Python LSP program."
    (jw/local-pyright-command))

    (defun jw/rust-lsp-program (&optional interactive)
    "Get Rust LSP program."
    (list (jw/find-rust-analyzer)))

    (defun jw/clangd-lsp-program (&optional interactive)
    "Get clangd LSP program."
    '("clangd"))

    (defun jw/typescript-lsp-program (&optional interactive)
    "Get TypeScript LSP program."
    '("typescript-language-server" "--stdio"))

    (defun jw/marksman-lsp-program (&optional interactive)
    "Get Marksman LSP program."
    '("marksman"))

    (defun jw/astro-lsp-program (&optional interactive)
    "Get Astro LSP program."
    '("astro-ls" "--stdio" :initializationOptions (:typescript (:tsdk "./node_modules/typescript/lib"))))


```



Add to `eglot` server list and setup hook after eglot is loaded.

```emacs-lisp

    ;; Enhanced eglot configuration
    (with-eval-after-load 'eglot
    (setq eglot-prefer-local-server t)
    ;; undo elgot modifications of completion-category-defaults
    (setq completion-category-defaults nil)
    (setq eglot-connect-timeout 120)

    ;; Use function symbols - eglot will call these functions to get the command
    (add-to-list 'eglot-server-programs
                '(python-ts-mode . jw/python-lsp-program))
    (add-to-list 'eglot-server-programs
                '(rust-mode . jw/rust-lsp-program))
    (add-to-list 'eglot-server-programs 
                '((c++-ts-mode c-ts-mode) . jw/clangd-lsp-program))
    (add-to-list 'eglot-server-programs
                '(typescript-ts-mode . jw/typescript-lsp-program))
    (add-to-list 'eglot-server-programs
                '(tsx-ts-mode . jw/typescript-lsp-program))
    (add-to-list 'eglot-server-programs 
                '(markdown-mode . jw/marksman-lsp-program))
    (add-to-list 'eglot-server-programs 
                '(astro-mode . jw/astro-lsp-program)))

```

Function to start eglot.

```emacs-lisp

  ;; Function to start eglot
    (defun jw/maybe-start-eglot ()
    "Start eglot if current mode is supported and file is not remote."
    (when (and (not (file-remote-p default-directory))
                (or (derived-mode-p 'python-mode)
                    (derived-mode-p 'python-ts-mode)
                    (derived-mode-p 'rust-mode)
                    (derived-mode-p 'c-ts-mode)
                    (derived-mode-p 'c++-ts-mode)
                    (derived-mode-p 'typescript-ts-mode)
                    (derived-mode-p 'tsx-ts-mode)
                    (derived-mode-p 'markdown-mode)
                    (derived-mode-p 'astro-mode)))
        (eglot-ensure)))

  ;; Helper function to restart eglot in current buffer
  (defun jw/restart-eglot ()
    "Restart eglot in current buffer."
    (interactive)
    (when (eglot-current-server)
      (eglot-shutdown (eglot-current-server))
      (eglot-ensure)))

```

Add the hook to auto start `eglot` depending on configured language.

```emacs-lisp

  (add-hook 'python-ts-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'rust-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'c-ts-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'c++-ts-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'typescript-ts-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'tsx-ts-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'markdown-mode-hook #'jw/maybe-start-eglot)
  (add-hook 'astro-mode-hook #'jw/maybe-start-eglot)

```


### The `jw-emacs-langs.el` section for debugging (`dape`)

```emacs-lisp

  (use-package dape
    :ensure t
    ;; :preface
    ;; By default dape shares the same keybinding prefix as `gud'
    ;; If you do not want to use any prefix, set it to nil.
    ;; (setq dape-key-prefix "\C-x\C-a")

    :hook
    ;; Save breakpoints on quit
    (kill-emacs . dape-breakpoint-save)
    ;; Load breakpoints on startup
    ;; (after-init . dape-breakpoint-load)

    :config
    ;; Turn on global bindings for setting breakpoints with mouse
    ;; (dape-breakpoint-global-mode)

    ;; Info buffers to the right
    (setq dape-buffer-window-arrangement 'right)

    ;; Info buffers like gud (gdb-mi)
    (setq dape-buffer-window-arrangement 'gud)
    (setq dape-info-hide-mode-line nil)

    ;; Pulse source line (performance hit)
    (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

    ;; Showing inlay hints
    (setq dape-inlay-hints t)

    ;; Save buffers on startup, useful for interpreted languages
    (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

    ;; Kill compile buffer on build success
    (add-hook 'dape-compile-hook 'kill-buffer)

    ;; Projectile users
    ;; (setq dape-cwd-function 'projectile-project-root)
    )

  ;; Enable repeat mode for more ergonomic `dape' use
  (use-package repeat
    :config
    (repeat-mode))

```


### The `jw-emacs-langs.el` for `tramp-sh.el` and remote configs

```emacs-lisp

  (with-eval-after-load 'tramp
    (require 'tramp-sh)
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

```


### The `jw-emacs-langs.el` call to provide

```emacs-lisp

  (provide 'jw-emacs-langs)

```

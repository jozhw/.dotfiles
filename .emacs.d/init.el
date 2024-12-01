(setq-default mode-line-format nil)

(require 'dired)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
            ;; Uncomment the next line to start with details hidden
            (dired-hide-details-mode 1)
            ))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(setq use-package-always-ensure t)

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(eval-and-compile
  (defvar use-package-selected-packages nil
   "Explicitly installed packages.")

  (define-advice use-package-handler/:ensure
      (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
               (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
              (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items))))))

  (define-advice use-package-handler/:vc
      (:around (fn name-symbol &rest rest) select)
    (cons `(add-to-list 'use-package-selected-packages ',name-symbol)
          (apply fn name-symbol rest))))

(define-advice use-package-handler/:init
  (:around (fn name-symbol keyword args rest state) select)
(let ((items (funcall fn name-symbol keyword args rest state)))
  (dolist (init args items)
    (push `(add-to-list 'use-package-selected-packages ',name-symbol) items))))

(defun use-package-autoremove ()
"Autoremove packages not used by use-package."
(interactive)
(let ((package-selected-packages use-package-selected-packages))
  (package-autoremove)))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package no-littering
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-create-definer jw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq-default indent-tabs-mode nil)

(jw/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Always start with *scratch*
(setq initial-buffer-choice t)

;; A few more useful configurations...
(use-package emacs
  :ensure t
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

(defcustom jw-emacs-load-which-key nil
  "When non-nil, display key binding hints after a short delay.
This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'jw-emacs
  :type 'boolean)

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
(require 'jw-emacs-langs)

(require 'jw-copy)

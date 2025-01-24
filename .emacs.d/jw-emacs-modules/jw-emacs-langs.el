(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq treesit-auto-install 'prompt)

(unless (package-installed-p 'conda)
  (package-refresh-contents)
  (package-install 'conda))

(require 'conda)
(setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/bin/pyright-langserver"))
(setq conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/bin/pyright-langserver"))
  (conda-env-autoactivate-mode t)

(defun jw/find-pyright-langserver ()
  (or (executable-find "pyright-langserver")
      (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/bin/pyright-langserver")
      ))

(defun jw/local-pyright-command ()
  "Return the command to run the local Pyright server."
  (let ((pyright-path (jw/find-pyright-langserver)))
    (if pyright-path
        (list pyright-path "--stdio")
      (error "Could not find pyright-langserver"))))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(unless (package-installed-p 'rust-mode)
  (package-refresh-contents)
  (package-install 'rust-mode))

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun jw/find-rust-analyzer ()
  (or (executable-find "rust-analyzer")
      (expand-file-name "~/.cargo/bin/rust-analyzer")))

;; (use-package typescript-mode
;; :ensure t
;; :mode "\\.ts\\'")

;; (use-package json-mode
;; :ensure t
;; :mode "\\.json\\'")

(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (apheleia-global-mode +1))

(with-eval-after-load 'eglot
  (setq eglot-prefer-local-server t)
  ;; undo elgot modifications of completion-category-defaults
  (setq completion-category-defaults nil)
  (setq eglot-connect-timeout 120)
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(jw/local-pyright-command)))
  (add-to-list 'eglot-server-programs
               `(rust-mode . (,(jw/find-rust-analyzer))))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  )

(defun jw/maybe-start-eglot ()
  (when (or (derived-mode-p 'python-mode)
            (derived-mode-p 'rust-mode)
            (derived-mode-p 'c-mode)
            (derived-mode-p 'c++-mode)
            (derived-mode-p 'typescript-ts-mode)
            (derived-mode-p 'tsx-ts-mode)
            )
    (eglot-ensure)))

(add-hook 'python-mode-hook #'jw/maybe-start-eglot)
(add-hook 'rust-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c++-mode-hook #'jw/maybe-start-eglot)
(add-hook 'typescript-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'tsx-ts-mode-hook #'jw/maybe-start-eglot)

(with-eval-after-load 'tramp
  (require 'tramp-sh)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'jw-emacs-langs)

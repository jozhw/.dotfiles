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
(setq conda-anaconda-home (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/"))
 (setq conda-env-home-directory (expand-file-name "/opt/homebrew/Caskroom/miniconda/base/envs/"))
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
  :hook (python-ts-mode . python-black-on-save-mode))

;; WEB MODE
(use-package web-mode
:ensure t)

;; astro
;; ASTRO
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
    (append '((".*\\.astro\\'" . astro-mode))
            auto-mode-alist))

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

;; Function to start eglot
(defun jw/maybe-start-eglot ()
  "Start eglot if current mode is supported."
  (when (or (derived-mode-p 'python-mode)
            (derived-mode-p 'python-ts-mode)
            (derived-mode-p 'rust-mode)
            (derived-mode-p 'c-ts-mode)
            (derived-mode-p 'c++-ts-mode)
            (derived-mode-p 'typescript-ts-mode)
            (derived-mode-p 'tsx-ts-mode)
            (derived-mode-p 'markdown-mode)
            (derived-mode-p 'astro-mode))
    (eglot-ensure)))

;; Helper function to restart eglot in current buffer
(defun jw/restart-eglot ()
  "Restart eglot in current buffer."
  (interactive)
  (when (eglot-current-server)
    (eglot-shutdown (eglot-current-server))
    (eglot-ensure)))

(add-hook 'python-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'rust-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c++-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'typescript-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'tsx-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'markdown-mode-hook #'jw/maybe-start-eglot)
(add-hook 'astro-mode-hook #'jw/maybe-start-eglot)

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

(with-eval-after-load 'tramp
  (require 'tramp-sh)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'jw-emacs-langs)

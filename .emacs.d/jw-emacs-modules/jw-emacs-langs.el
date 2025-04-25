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
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        ))

(dolist (source treesit-language-source-alist)
  (unless (treesit-ready-p (car source))
    (treesit-install-language-grammar (car source))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
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

(with-eval-after-load 'eglot
  (setq eglot-prefer-local-server t)
  ;; undo elgot modifications of completion-category-defaults
  (setq completion-category-defaults nil)
  (setq eglot-connect-timeout 120)
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(jw/local-pyright-command)))
  (add-to-list 'eglot-server-programs
               `(rust-mode . (,(jw/find-rust-analyzer))))
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  )

(defun jw/maybe-start-eglot ()
  (when (or (derived-mode-p 'python-mode)
            (derived-mode-p 'python-ts-mode)
            (derived-mode-p 'rust-mode)
            (derived-mode-p 'c-ts-mode)
            (derived-mode-p 'c++-ts-mode)
            (derived-mode-p 'typescript-ts-mode)
            (derived-mode-p 'tsx-ts-mode)
            )
    (eglot-ensure)))

(add-hook 'python-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'rust-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'c++-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'typescript-ts-mode-hook #'jw/maybe-start-eglot)
(add-hook 'tsx-ts-mode-hook #'jw/maybe-start-eglot)

(with-eval-after-load 'tramp
  (require 'tramp-sh)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'jw-emacs-langs)

(with-eval-after-load 'eglot
   (setq completion-category-defaults nil))

(unless (package-installed-p 'conda)
  (package-refresh-contents)
  (package-install 'conda))

(require 'conda)
(setq conda-anaconda-home (expand-file-name "~/miniconda3"))
(setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  (conda-env-autoactivate-mode t)

(defun jw/find-pyright-langserver ()
  (or (executable-find "pyright-langserver")
      (expand-file-name "~/miniconda3/bin/pyright-langserver")))

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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . (,(jw/find-pyright-langserver) "--stdio")))
  (add-to-list 'eglot-server-programs
               `(rust-mode . (,(jw/find-rust-analyzer))))
  )

(defun jw/maybe-start-eglot ()
  (when (or (derived-mode-p 'python-mode)
            (derived-mode-p 'rust-mode))
    (eglot-ensure)))

(add-hook 'python-mode-hook #'jw/maybe-start-eglot)
(add-hook 'rust-mode-hook #'jw/maybe-start-eglot)

(with-eval-after-load 'tramp
  (require 'tramp-sh)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(provide 'jw-emacs-langs)

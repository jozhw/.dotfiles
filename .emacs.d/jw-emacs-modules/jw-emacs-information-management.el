(use-package denote
                 :ensure t)

(setq denote-directory (expand-file-name "~/Otzar/Gnosis/"))
(setq denote-save-buffer-after-creation nil)

(setq denote-known-keywords '("theology" "philosophy" "politics" "journal" "analysis" "linguistics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)

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
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

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

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq lock-file-name-transforms
    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(provide 'jw-emacs-information-management)

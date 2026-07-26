
(use-package denote
                 :straight t)

(setq denote-directory (expand-file-name "~/Core/Otzar/Gnosis/"))
;; Create the notes directory if it does not exist yet (e.g. fresh machine).
(unless (file-directory-p denote-directory)
  (make-directory denote-directory t))
(setq denote-save-buffer-after-creation nil)



(add-hook 'dired-mode-hook #'denote-dired-mode)



(setq denote-known-keywords
      '("math" "markets" "hf" "code" "infra"
        "philosophy" "ministry" "health" "people" "writing"))
;; Offer keywords already present in the notes, not only the list above, so
;; the vocabulary can drift without editing this file.
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



;; Ensure denote.el is loaded
(require 'denote)

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               `("t" "Todo (today)" entry
                 (file ,jw-org-todo-file)
                 "* TODO %?\nSCHEDULED: %t"
                 :empty-lines 1)))



(use-package obsidian
  :straight t
  :demand t
  :config
  (let ((vault (expand-file-name "~/Core/Otzar/Obsidian/")))
    (dolist (dir (list vault
                       (expand-file-name "notes" vault)
                       (expand-file-name "daily-notes" vault)
                       (expand-file-name "templates" vault)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    ;; `obsidian-daily-note' inserts this file without checking that it
    ;; exists, so seed a minimal one.
    (let ((template (expand-file-name "templates/Daily Note Template.md" vault)))
      (unless (file-exists-p template)
        (with-temp-file template
          (insert "# {{title}}\n\n"))))
    (setopt obsidian-directory vault))
  ;; Track vault files everywhere so links and tags resolve globally.
  (global-obsidian-mode t))



(setq obsidian-inbox-directory "notes")             ; destination for `obsidian-capture'
(setq obsidian-daily-notes-directory "daily-notes") ; daily note file is YYYY-MM-DD.md
(setq obsidian-templates-directory "templates")     ; note templates live here
(setq obsidian-daily-note-template "Daily Note Template.md")
;; Following a wiki-link to a note that does not exist yet creates it in
;; `obsidian-inbox-directory' rather than beside the current file.
(setq obsidian-create-unfound-files-in-inbox t)



;; Entry points: reachable from anywhere, not only from inside the vault.
(global-set-key (kbd "C-c n n") #'obsidian-daily-note)
(global-set-key (kbd "C-c n c") #'obsidian-capture)
(global-set-key (kbd "C-c n j") #'obsidian-jump)
(global-set-key (kbd "C-c n s") #'obsidian-search)
(global-set-key (kbd "C-c n t") #'obsidian-insert-tag)
(global-set-key (kbd "C-c n f") #'obsidian-find-tag)
(global-set-key (kbd "C-c n i") #'jw-obsidian-insert-template)
(global-set-key (kbd "C-c n b") #'obsidian-backlinks-mode)
(global-set-key (kbd "C-c n u") #'obsidian-update)

(with-eval-after-load 'obsidian
  (define-key obsidian-mode-map (kbd "C-c C-o") #'obsidian-follow-link-at-point)
  (define-key obsidian-mode-map (kbd "C-c C-b") #'obsidian-backlink-jump)
  (define-key obsidian-mode-map (kbd "C-c C-l") #'obsidian-insert-wikilink))



(use-package xeft
  :straight t
  :after obsidian
  :bind ("C-c n g" . xeft)
  :custom
  (xeft-directory obsidian-directory)
  (xeft-recursive t)                            ; notes/, daily-notes/, ...
  (xeft-file-filter #'obsidian-file-p)
  (xeft-title-function #'obsidian-file-title-function))



(defun jw-obsidian-insert-template ()
  "Insert a template from `obsidian-templates-directory' into this buffer.
Substitutes {{title}}, {{date}} and {{time}} the same way `obsidian-daily-note'
does, since it reuses `obsidian-apply-template'."
  (interactive)
  (let* ((dir (expand-file-name obsidian-templates-directory obsidian-directory))
         (templates (directory-files dir nil "\\.md\\'")))
    (unless templates
      (user-error "No templates in %s" dir))
    (obsidian-apply-template
     (expand-file-name (completing-read "Template: " templates) dir))))



(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq lock-file-name-transforms
    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))



(provide 'jw-emacs-information-management)


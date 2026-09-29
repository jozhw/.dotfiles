;;; jw-emacs-information-management.el --- Notes and information management -*- lexical-binding: t; -*-

(defvar jw-otzar-directory (expand-file-name "~/Core/otzar/")
  "Root of the knowledge base.")

(use-package denote
  :straight t
  :demand t)

(setq denote-directory (expand-file-name "synthesis/" jw-otzar-directory)
      denote-save-buffers nil)
(dolist (dir '("synthesis" "transcripts" "assets/recordings"))
  (make-directory (expand-file-name dir jw-otzar-directory) t))

(use-package denote-markdown
  :straight (denote-markdown :type git :host github :repo "protesilaos/denote-markdown")
  :commands (denote-markdown-convert-links-to-file-paths
             denote-markdown-convert-links-to-denote-type))

(add-hook 'dired-mode-hook #'denote-dired-mode)

(setq denote-known-keywords
      '("math" "markets" "hf" "code" "infra"
        "philosophy" "ministry" "health" "people" "writing"))
;; Offer keywords already present in the notes, not only the list above, so
;; the vocabulary can drift without editing this file.
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)

(setq denote-file-type 'markdown-yaml
      denote-prompts '(title keywords)
      denote-excluded-directories-regexp nil
      denote-excluded-keywords-regexp nil
      denote-date-prompt-use-org-read-date t
      denote-date-format nil
      denote-backlinks-show-context t)

(add-hook 'find-file-hook #'denote-fontify-links-mode-maybe)
(add-hook 'context-menu-functions #'denote-context-menu)

(defun jw-denote-daily-note ()
  "Open today's note, creating a timestamp-only Markdown file if needed."
  (interactive)
  (let* ((now (current-time))
         (day (format-time-string "%Y%m%d" now))
         (directory (file-name-as-directory denote-directory))
         (regexp (concat "\\`" day "T[0-9]\\{6\\}\\.md\\'")))
    (make-directory directory t)
    (if-let* ((file (car (directory-files directory t regexp))))
        (find-file file)
      (let ((denote-file-name-components-order '(identifier))
            (denote-save-buffers nil)
            (denote-kill-buffers nil))
        (find-file (denote "" nil 'markdown-yaml directory
                           (format-time-string "%Y-%m-%d %H:%M:%S" now)))
        (goto-char (point-max))
        (insert "# " (format-time-string "%Y-%m-%d" now) "\n\n")
        (save-buffer)))))

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
  (let ((vault jw-otzar-directory))
    (dolist (dir (list vault
                       (expand-file-name "gnosis" vault)
                       (expand-file-name "templates" vault)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (pcase-dolist (`(,name . ,body)
                   '(("Note.md" . "---\ncreated: {{date}}\ntags: []\n---\n\n# {{title}}\n\n<!-- the claim, in one sentence -->\n\n## Why\n\n## Sources\n")))
      (let ((file (expand-file-name (concat "templates/" name) vault)))
        (unless (file-exists-p file)
          (with-temp-file file (insert body)))))
    (setopt obsidian-directory vault))
  ;; Track vault files everywhere so links and tags resolve globally.
  (global-obsidian-mode t))

(setq obsidian-inbox-directory "gnosis"
      obsidian-daily-notes-directory nil
      obsidian-templates-directory "templates"
      obsidian-daily-note-template nil
      obsidian-create-unfound-files-in-inbox t)

(setq-default markdown-enable-math t)

;; Entry points: reachable from anywhere, not only from inside the vault.
(global-set-key (kbd "C-c n n") #'jw-denote-daily-note)
(global-set-key (kbd "C-c n N") #'denote)
(global-set-key (kbd "C-c n c") #'jw-obsidian-capture)
(global-set-key (kbd "C-c n j") #'obsidian-jump)
(global-set-key (kbd "C-c n s") #'obsidian-search)
(global-set-key (kbd "C-c n t") #'jw-obsidian-add-tag)
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
  (xeft-recursive t)
  (xeft-file-filter #'obsidian-file-p)
  (xeft-title-function #'obsidian-file-title-function))

(defvar jw-obsidian-note-template "Note.md"
  "Template in `obsidian-templates-directory' applied by `jw-obsidian-capture'.")

(defun jw-obsidian-capture ()
  "Capture a note like `obsidian-capture', then apply `jw-obsidian-note-template'.
`obsidian-capture' applies no template -- only `obsidian-daily-note' does --
so a captured note would otherwise start with no front matter at all."
  (interactive)
  (call-interactively #'obsidian-capture)
  (when (and obsidian-templates-directory
             jw-obsidian-note-template
             (eq (buffer-size) 0))
    (obsidian-apply-template
     (expand-file-name jw-obsidian-note-template
                       (expand-file-name obsidian-templates-directory
                                         obsidian-directory)))
    (save-buffer)))

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

(defun jw-obsidian-add-tag (tag)
  "Add TAG to the front-matter `tags:' list, completing on tags in the vault.
Merges into the bracketed list rather than inserting at point, so the list
stays comma-separated and free of duplicates.  Falls back to inserting an
inline #TAG at point when the buffer has no front-matter `tags:' list.
Vault tags carry no leading `#', per the `obsidian-tags' docstring."
  (interactive
   (list (completing-read "Tag: " (sort (obsidian-tags) #'string<))))
  (let ((merged
         (save-excursion
           (goto-char (point-min))
           (when (looking-at-p "^---[ \t]*$")
             (forward-line 1)
             (when-let* ((end (save-excursion
                               (re-search-forward "^---[ \t]*$" nil t))))
               (when (re-search-forward "^tags:[ \t]*\\[\\([^]]*\\)\\]" end t)
                 (let* ((current (split-string (match-string 1) "[,[:space:]]+" t))
                        (all (delete-dups (append current (list tag)))))
                   (replace-match
                    (concat "tags: [" (mapconcat #'identity all ", ") "]")
                    t t)
                   t)))))))
    (unless merged
      (insert (format "#%s" tag)))))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq lock-file-name-transforms
    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(provide 'jw-emacs-information-management)

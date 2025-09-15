(use-package denote
                 :straight t)

(setq denote-directory (expand-file-name "~/Otzar/Gnosis/"))
(setq denote-save-buffer-after-creation nil)

(add-hook 'dired-mode-hook #'denote-dired-mode)

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

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(setq lock-file-name-transforms
    '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))

(provide 'jw-emacs-information-management)

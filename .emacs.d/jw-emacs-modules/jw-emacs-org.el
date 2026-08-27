;;; jw-emacs-org.el --- Org mode configuration -*- lexical-binding: t; -*-

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




(defvar jw-org-todo-file
  (expand-file-name "~/Core/Otzar/Docs/agenda/todo.org")
  "The one and only agenda file.
Referenced by the capture template in `jw-emacs-information-management'.")

;; Create the directory and the file on a fresh machine so that both
;; `org-agenda' and `org-capture' work without any manual setup.
(let ((agenda-dir (file-name-directory jw-org-todo-file)))
  (unless (file-directory-p agenda-dir)
    (make-directory agenda-dir t)))
(unless (file-exists-p jw-org-todo-file)
  (with-temp-file jw-org-todo-file
    (insert "#+title: Todo\n\n")))

(setq org-agenda-files (list jw-org-todo-file))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))

;; Show today, not the week ahead -- the agenda is a day's worklist, not a
;; project plan.
(setq org-agenda-span 'day)
(setq org-agenda-start-on-weekday nil)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)



(defvar jw-org-archive-directory
  (expand-file-name "~/Core/Otzar/Docs/agenda/archive/")
  "Directory holding one Org archive file per year.")

(defun jw-org-archive-location-for-entry ()
  "Return an `org-archive-location' for the finished entry at point.
The year is taken from the entry's CLOSED timestamp so that a late sweep
still files work under the year it was actually finished."
  (let* ((closed (org-entry-get nil "CLOSED" t))
         (year (format-time-string
                "%Y"
                (if closed (org-time-string-to-time closed) (current-time)))))
    (concat (expand-file-name (concat year ".org") jw-org-archive-directory)
            "::datetree/")))

(defun jw-org-archive-done ()
  "Archive every finished entry in `jw-org-todo-file' to the year datetrees.
`org-entry-is-done-p' tests membership in `org-done-keywords', so both DONE
and CANCEL qualify -- they sit after the `|' in `org-todo-keywords'."
  (interactive)
  (unless (file-directory-p jw-org-archive-directory)
    (make-directory jw-org-archive-directory t))
  (with-current-buffer (find-file-noselect jw-org-todo-file)
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (when (org-entry-is-done-p)
           ;; Bound per entry, not globally: see the note above.
           (let ((org-archive-location (jw-org-archive-location-for-entry)))
             (org-archive-subtree))
           (setq count (1+ count))
           ;; `org-archive-subtree' removes the entry, which leaves the
           ;; mapper's saved position stale; `org-map-continue-from' is the
           ;; documented way to tell it where to resume.
           (setq org-map-continue-from (point))))
       t 'file)
      (save-buffer)
      (message "Archived %d finished %s" count
               (if (= count 1) "entry" "entries")))))

(global-set-key (kbd "C-c A") #'jw-org-archive-done)



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




(setq org-clock-sound "~/.dotfiles/.assets/sounds/mixkit-alert-quick-chime-766.wav")



(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)



(setq org-id-link-to-org-use-id 'create-if-interactive)



(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'org-display-inline-images)



;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("clang" . "src c"))
(add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))



;; Automatically tangle our Emacs.org config file when we save it
(defun jw/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'jw/org-babel-tangle-config)))



(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)



(defun jw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jw/org-mode-visual-fill)
  (markdown-mode . jw/org-mode-visual-fill))



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



(provide 'jw-emacs-org)


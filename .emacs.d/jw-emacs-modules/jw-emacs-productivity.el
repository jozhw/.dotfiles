(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  :hook (pdf-view-mode . (lambda () 
                       (display-line-numbers-mode -1)
                       (message "PDF Tools activated for this buffer"))))

;; Ensure org-noter is installed
(use-package org-noter
  :straight t
  :after (org pdf-tools)
  :config
  (setq org-noter-always-create-frame nil))

;; Ensure org-pdftools is set up to work with org-mode
(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

;; Configure org-noter-pdftools
(use-package org-noter-pdftools
  :after (org-noter pdf-tools)
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; Fix for the specific issue
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
  With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))

  ;; Add a hook for pdf-annot
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))

  ;; If you are working with EPUB files
  (use-package nov
    :straight t)

  ;; If you are working with DJVU files
  (use-package djvu
    :straight t))

(provide 'jw-emacs-productivity)

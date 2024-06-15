;;; Theme setup and related

;;;; Load the desired theme module
;; These all reference my packages: `modus-themes', `ef-themes',
;; `standard-themes'.
(when jw-emacs-load-theme-family
  (require
   (pcase jw-emacs-load-theme-family
     ('ef 'jw-emacs-ef-themes)
     ('modus 'jw-emacs-modus-themes)
     ('standard 'jw-emacs-standard-themes))))


(provide 'jw-emacs-theme)

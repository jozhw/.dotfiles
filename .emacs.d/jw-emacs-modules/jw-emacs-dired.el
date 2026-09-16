;;; jw-emacs-dired.el --- Dired configuration -*- lexical-binding: t; -*-

(setq dired-listing-switches "-alD")



(setq insert-directory-program "gls" 
     dired-use-ls-dired t)



(provide 'jw-emacs-dired)


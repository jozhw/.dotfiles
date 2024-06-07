;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(add-to-list 'load-path (expand-file-name "jw-emacs-modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "jw-lisp" user-emacs-directory))

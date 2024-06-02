;; Disable startup message when emacs starts
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell to be on instead of the beeping
;; uncomment below to have the visual bell, otherwise keep the code commented
;; (setq visible-bell t)

;; (setq visible-bell t)

;; Enable column numbers
(column-number-mode)

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default fill-column 80)

(provide 'jw-emacs-essentials)

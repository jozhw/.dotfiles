(defvar jw-emacs-tiling-window-manager-regexp "bspwm\\|herbstluftwm\\|i3"
  "Regular expression to  tiling window managers.
See definition of `prot-emacs-with-desktop-session'.")

(defmacro jw-emacs-with-desktop-session (&rest body)
  "Expand BODY if desktop session is not a tiling window manager.
See `prot-emacs-tiling-window-manager-regexp' for what
constitutes a matching tiling window manager."
  (declare (indent 0))
  `(when-let ((session (getenv "DESKTOP_SESSION"))
              ((not (string-match-p session jw-emacs-tiling-window-manager-regexp))))
     ,@body))

;; Set frame parameters early (without font-related settings;; )
(setq initial-frame-alist
      (append '((alpha . (90 . 90))
                (fullscreen . maximized))
              initial-frame-alist))

(setq default-frame-alist
      (append '((alpha . (90 . 90))
                (fullscreen . maximized))
              default-frame-alist))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

(defun jw-emacs-theme-gsettings-dark-p ()
  "Return non-nil if gsettings (GNOME) has a dark theme.
Return nil if the DESKTOP_SESSION is either bspwm or
herbstluftwm, per the configuration of my dotfiles.  Also check
the `delight' shell script."
  (jw-emacs-with-desktop-session
    (string-match-p
     "dark"
     (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))))

(defun jw-emacs-theme-twm-dark-p ()
  "Return non-nil if my custom setup has a dark theme.
I place a file in ~/.config/prot-xtwm-active-theme which contains
a single word describing my system-wide theme.  This is part of
my dotfiles.  Check my `delight' shell script for more."
  (when-let ((file "~/.config/jw-xtwm-active-theme")
             ((file-exists-p file)))
      (string-match-p
       "dark"
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string)))))

(defun jw-emacs-theme-environment-dark-p ()
  "Return non-nil if environment theme is dark."
  (or (jw-emacs-theme-twm-dark-p)
      (jw-emacs-theme-gsettings-dark-p)))

(defun jw-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`prot-emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(add-to-list 'load-path (expand-file-name "jw-emacs-modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "jw-lisp" user-emacs-directory))

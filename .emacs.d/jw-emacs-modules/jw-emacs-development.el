(use-package tramp
  :straight t)
(setq tramp-default-method "ssh")
;; for debugging
(setq tramp-verbose 3)
;; some basic performance enhancements
(setq remote-file-name-inhibit-locks t
    tramp-use-scp-direct-remote-copying t
    remote-file-name-inhibit-auto-save-visited t)
;; disable backup files for tramp
(add-to-list 'backup-directory-alist
            (cons tramp-file-name-regexp nil))

(setq tramp-connection-timeout 10) ;; 10 sec timeout

(connection-local-set-profile-variables
'remote-direct-async-process
'((tramp-direct-async-process . t)))

(connection-local-set-profiles
'(:application tramp :protocol "scp")
'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(with-eval-after-load 'tramp
(with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(setq password-cache-expiry nil)

;; Configure TRAMP to use ~/.emacs.d/tmp/ for caching
(let ((tramp-tmp-dir (expand-file-name "tmp/" user-emacs-directory)))
;; Ensure the directory exists
(unless (file-directory-p tramp-tmp-dir)
    (make-directory tramp-tmp-dir t))

;; Set TRAMP cache directory
(setq tramp-persistency-file-name (expand-file-name "tramp-cache" tramp-tmp-dir))

;; Set auto-save directory for remote files
(setq tramp-auto-save-directory tramp-tmp-dir)

;; Optional: Set backup directory for remote files to tmp as well
(setq tramp-backup-directory-alist `(("." . ,tramp-tmp-dir))))

;; Enable persistent caching
(setq tramp-cache-read-persistent-data t)
(setq tramp-cache-compress t)
(setq remote-file-name-inhibit-cache nil)

(defun memoize-remote (key cache orig-fn &rest args)
"Memoize a value if the key is a remote path."
(if (and key
        (file-remote-p key))
    (if-let ((current (assoc key (symbol-value cache))))
        (cdr current)
        (let ((current (apply orig-fn args)))
        (set cache (cons (cons key current) (symbol-value cache)))
        current))
    (apply orig-fn args)))

;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
(memoize-remote (or directory
                    project-current-directory-override
                    default-directory)
                'project-current-cache orig prompt directory))
(advice-add 'project-current :around #'memoize-project-current)

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
(memoize-remote (or directory default-directory)
                'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
(let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
    (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)

;; memoize all git candidates in the current project
(defvar $counsel-git-cands-cache nil)
(defun $memoize-counsel-git-cands (orig dir)
($memoize-remote (magit-toplevel dir) '$counsel-git-cands-cache orig dir))
(advice-add 'counsel-git-cands :around #'$memoize-counsel-git-cands)

;; Optional: Function to clear cache when needed
(defun jw/clear-tramp-cache ()
"Clear TRAMP cache files in ~/.emacs.d/tmp/"
(interactive)
(let ((cache-file tramp-persistency-file-name))
    (when (file-exists-p cache-file)
    (delete-file cache-file)
    (message "TRAMP cache cleared"))))

(require 'project)

(defun jw/project-prompter ()
     (read-file-name "Select a project folder:"
                     "~/Otzar/Projects/Code/"
                     nil
                     nil
                     nil
                     #'file-directory-p))
(setq project-prompter #'jw/project-prompter)

(show-paren-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode t)

(add-hook 'org-mode-hook (lambda ()
         (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package command-log-mode)

(provide 'jw-emacs-development)

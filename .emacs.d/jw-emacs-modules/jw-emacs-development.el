(require 'project)

(defun jw/project-prompter ()
     (read-file-name "Select a project folder:"
                     "~/Projects/Code/"
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
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package command-log-mode)

(provide 'jw-emacs-development)

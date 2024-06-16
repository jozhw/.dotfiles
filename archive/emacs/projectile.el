(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects-code")
    (setq projectile-project-search-path '("~/projects-code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

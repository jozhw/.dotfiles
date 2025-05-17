(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/OrgRoamTest/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  If using org-roam-protocol
  (require 'org-roam-protocol))

(setq zettlekasten-paths-alist '(("Main" . "~/Trove/Epektasi/")
                                 ("Zettlekasten" . "~/Trove/Zettlekasten/")
                                 ("Test" . "~/OrgRoamTest/")))

(defun switch-zettlekasten ()
  (interactive)
  (let* ((keys (mapcar #'car zettlekasten-paths-alist))
         (prompt (format "Select Zettlekasten:"))
         (key (completing-read prompt keys))
         (chosen-zettlekasten-path (cdr (assoc key zettlekasten-paths-alist))))
    (setq org-roam-directory chosen-zettlekasten-path)
    (setq org-roam-db-location (concat chosen-zettlekasten-path "org-roam.db"))
    (org-roam-db-sync)))

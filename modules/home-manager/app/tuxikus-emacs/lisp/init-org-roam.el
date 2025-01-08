;; init-org-roam.el --- -*- lexical-binding: t -*-

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  :bind (("C-c o r b" . org-roam-buffer-toggle)
         ("C-c o r f" . org-roam-node-find)
         ("C-c o r g" . org-roam-graph)
         ("C-c o r i" . org-roam-node-insert)
         ("C-c o r c" . org-roam-capture)
         ;; Dailies
         ("C-c o r d" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-org-roam)

;; init-org-roam.el ends here

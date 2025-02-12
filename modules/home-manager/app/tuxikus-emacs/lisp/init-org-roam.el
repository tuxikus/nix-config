;;; init-org-roam.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-org-roam)

;;; init-org-roam.el ends here

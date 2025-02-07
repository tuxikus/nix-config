;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :init
  (setq org-attach-id-dir "~/org/.attach"
	org-log-done 'time
	org-hide-emphasis-markers t)
  :config
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.4)
  (set-face-attribute 'org-level-3 nil :height 1.3)
  (set-face-attribute 'org-level-4 nil :height 1.2)
  (set-face-attribute 'org-level-5 nil :height 1.1)
  (set-face-attribute 'org-level-6 nil :height 1.0)
  (set-face-attribute 'org-level-7 nil :height 1.0)
  (set-face-attribute 'org-level-8 nil :height 1.0)
  (set-face-attribute 'org-block-begin-line nil :background "#f0f0f0")
  (set-face-attribute 'org-block-end-line nil :background "#f0f0f0")
  (set-face-attribute 'org-document-title nil :height 2.0))

(provide 'init-org)

;;; init-org.el ends here

;; config-org.el --- -*- lexical-binding: t -*-

(use-package org
  :bind
  (("C-c o t" . 'org-todo))
  :init
  (setq org-attach-id-dir "~/org/.attach"
	org-log-done 'time))

(provide 'config-org)

;; config-org.el ends here

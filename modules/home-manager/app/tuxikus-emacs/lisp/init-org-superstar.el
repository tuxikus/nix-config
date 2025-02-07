;;; init-org-superstar.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

(provide 'init-org-superstar)

;;; init-org-superstar.el ends here

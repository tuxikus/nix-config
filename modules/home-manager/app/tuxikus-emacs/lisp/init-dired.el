;;; init-dired.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode 1))))

(provide 'init-dired)

;;; init-dired.el ends here

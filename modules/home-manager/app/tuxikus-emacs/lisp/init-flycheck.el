;; init-flycheck.el --- -*- lexical-binding: t -*-

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-flycheck)

;; init-flycheck.el ends here

;; init-all-the-icons.el --- -*- lexical-binding: t -*-

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :config (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'init-all-the-icons)

;; init-all-the-icons.el ends here

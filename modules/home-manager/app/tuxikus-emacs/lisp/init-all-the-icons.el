;; init-all-the-icons.el --- -*- lexical-binding: t -*-

(use-package all-the-icons
  :if (display-graphic-p))

(define-minor-mode all-the-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if all-the-icons-completion-mode
      (advice-add (compat-function completion-metadata-get) :around #'all-the-icons-completion-completion-metadata-get)
    (advice-remove (compat-function completion-metadata-get) #'all-the-icons-completion-completion-metadata-get)))

(use-package all-the-icons-completion
      :after (marginalia all-the-icons)
      :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
      :init
      (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'init-all-the-icons)

;; init-all-the-icons.el ends here

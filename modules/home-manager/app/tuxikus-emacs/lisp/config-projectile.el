;; config-projectile.el --- -*- lexical-binding: t -*-

(use-package projectile
  :bind (("C-c p p" . projectile-switch-project)
	 ("C-c p a" . projectile-add-known-project)
	 ("C-c p f" . projectile-find-file)
	 ("C-c p s" . projectile-ripgrep))
  :init
  (projectile-mode +1))

(provide 'config-projectile)

;; config-projectile.el ends here

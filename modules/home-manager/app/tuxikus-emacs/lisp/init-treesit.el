;; init-treesit.el --- -*- lexical-binding: t -*-

(use-package treesit
  :init
  (setq major-mode-remap-alist
	'((bash-mode . bash-ts-mode)
	  (python-mode . python-ts-mode))))

(provide 'init-treesit)

;; init-treesit.el ends here

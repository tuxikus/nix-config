;; config-corfu.el --- -*- lexical-binding: t -*-

(use-package corfu
  :init
  (setq corfu-auto t
	corfu-quit-no-match t
	corfu-quit-at-boundary 'seperator)
  (global-corfu-mode))

(provide 'config-corfu)

;; config-corfu.el ends here

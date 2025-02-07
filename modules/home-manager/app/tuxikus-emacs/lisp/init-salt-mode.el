;;; init-salt-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package salt-mode
  :hook
  (salt-mode . (lambda () (flyspell-mode 1))))

(provide 'init-salt-mode)

;;; init-salt-mode.el ends here

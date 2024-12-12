;; config-dired.el --- -*- lexical-binding: t -*-

;; less verbose
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;; enable a
(put 'dired-find-alternate-file 'disabled nil)

(provide 'config-dired)

;; config-dired.el ends here

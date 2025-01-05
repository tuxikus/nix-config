;; config-cape.el --- -*- lexical-binding: t -*-

(use-package cape
  :bind ("M-TAB" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
)

(provide 'config-cape)

;; config-cape.el ends here

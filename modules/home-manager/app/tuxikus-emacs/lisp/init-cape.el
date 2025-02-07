;;; init-cape.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cape
  :bind ("M-TAB" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(provide 'init-cape)

;;; init-cape.el ends here

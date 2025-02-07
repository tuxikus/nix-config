;;; init-marginalia.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'init-marginalia)

;;; init-marginalia.el ends here

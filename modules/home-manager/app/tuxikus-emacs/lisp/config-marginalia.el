;; config-marginalia.el --- -*- lexical-binding: t -*-

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'config-marginalia)

;; config-marginalia.el ends here

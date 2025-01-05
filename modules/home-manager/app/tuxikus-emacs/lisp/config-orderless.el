;; config-orderless.el --- -*- lexical-binding: t -*-

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(provide 'config-orderless)

;; config-orderless.el ends here

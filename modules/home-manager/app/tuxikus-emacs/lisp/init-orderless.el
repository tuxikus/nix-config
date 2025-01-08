;; init-orderless.el --- -*- lexical-binding: t -*-

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-orderless)

;; init-orderless.el ends here

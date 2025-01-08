;; init-ace-window.el --- -*- lexical-binding: t -*-

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))

(provide 'init-ace-window)

;; init-ace-window.el ends here

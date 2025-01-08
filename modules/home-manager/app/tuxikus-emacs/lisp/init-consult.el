;; init-consult.el --- -*- lexical-binding: t -*-

(use-package consult
  :bind (("C-c b s" . consult-buffer)
	 ("C-c l s" . consult-line)
	 ("C-c r g" . consult-ripgrep)))

(provide 'init-consult)

;; init-consult.el ends here

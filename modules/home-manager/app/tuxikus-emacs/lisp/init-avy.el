;;; init-avy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :bind (("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("C-'" . avy-goto-char-2)))

(provide 'init-avy)

;;; init-avy.el ends here

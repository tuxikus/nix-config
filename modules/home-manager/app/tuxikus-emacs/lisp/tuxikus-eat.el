;; tuxikus-eat.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun tuxikus/eat-bash ()
  "Launch eat with bash."
  (interactive)
  (eat "bash"))

(defun tuxikus/eat-zsh ()
  "Launch eat with zsh."
  (interactive)
  (eat "zsh"))

(defun tuxikus/eat-fish ()
  "Launch eat with fish."
  (interactive)
  (eat "fish"))

(provide 'tuxikus-eat)
;;; tuxikus-eat.el ends here

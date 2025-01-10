;; tuxikus-eat.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun tuxikus/eat-bash ()
  "Launch eat with bash."
  (interactive)
  (eat "bash"))

(defun tuxikus/eat-bash-other-window ()
  "Launch eat with bash in a other window."
  (eat-other-window "bash"))

(defun tuxikus/eat-zsh ()
  "Launch eat with zsh."
  (interactive)
  (eat "zsh"))

(defun tuxikus/eat-zsh-other-window ()
  "Launch eat with zsh in a other window."
  (interactive)
  (eat-other-window "zsh"))

(defun tuxikus/eat-fish ()
  "Launch eat with fish."
  (interactive)
  (eat "fish"))

(defun tuxikus/eat-fish-other-window ()
  "Launch eat with fish in a other window."
  (interactive)
  (eat-other-window "fish"))

(provide 'tuxikus-eat)
;;; tuxikus-eat.el ends here

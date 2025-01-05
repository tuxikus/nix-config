;; config-emacs.el --- -*- lexical-binding: t -*-

(use-package emacs
  :bind (("C-c f f" . find-file)
	("C-c f r" . recentf)
	("C-c f s" . save-buffer)

	("C-c b i" . ibuffer)
	("C-c b k" . kill-buffer)
	("C-c b d" . kill-buffer)

	("C-z" . undo)
	("C-S-z" . undo-redo))
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'config-emacs)

;; config-emacs.el ends here

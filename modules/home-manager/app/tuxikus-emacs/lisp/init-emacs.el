;;; init-emacs.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :init
  (setq create-lockfiles nil
	make-backup-files nil
	custom-theme-directory "~/.emacs.d/themes"
	inhibit-startup-message t
	inhibit-startup-screen t
	initial-scratch-message ";;; Emacs is fun")
  (fset 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (load-theme 'almost-mono-white t)
  (set-face-attribute 'default nil
                    :family "Iosevka Nerd Font"
                    :height 150
                    :weight 'light
                    :width 'normal)

  (set-face-attribute 'bold nil
		      :family "Iosevka Nerd Font"
		      :weight 'light)

  (set-face-attribute 'italic nil
		      :family "Iosevka Nerd Font"
		      :slant 'italic
		      :weight 'light)

  (set-face-attribute 'bold-italic nil
		      :family "Iosevka Nerd Font"
		      :weight 'light
		      :slant 'italic)

  ;; window divider
  (setq window-divider-default-right-width 5
	window-divider-default-bottom-width 5
	window-divider-default-places t)

  (window-divider-mode 1)
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'init-emacs)

;;; init-emacs.el ends here

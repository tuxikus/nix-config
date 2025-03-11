{ config, pkgs, lib, ... }:
let
  my-emacs = config.emacsPkg.override {
    withNativeCompilation = true;
  };
  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages ( epkgs: with epkgs; [
    ace-window
    almost-mono-themes
    avy
    cape
    consult
    consult-yasnippet
    corfu
    corfu-terminal
    dashboard
    direnv
    docker
    doom-modeline
    doom-themes
    dwim-shell-command
    eat
    embark
    embark-consult
    embark-org-roam
    ess
    fireplace
    flycheck
    keycast
    libmpdel
    magit
    marginalia
    move-text
    mpdel
    nix-mode
    orderless
    org-roam
    org-modern
    org-superstar
    org-present
    perspective
    python-mode
    pyvenv
    ripgrep
    salt-mode
    verb
    vertico
    vertico-posframe
    vundo
    walkman
    wgrep
    yasnippet
    (treesit-grammars.with-grammars (grammars: with grammars; [
      tree-sitter-python
      tree-sitter-bash
    ]))
  ]);
in
{
  options = {
    emacsPkg = lib.mkOption {
	type = lib.types.package;
    };
    customInit = lib.mkOption {
	type = lib.types.str;
    };
    fontSize = lib.mkOption {
	type = lib.types.str;
    };
  };

  config = {
    programs.emacs = {
      enable = true;
      package = my-emacs-with-packages;
      extraConfig = ''
        (load-file "~/.emacs.d/init.el")
      '';
    };
    home = {
      file = {
        ".emacs.d/init.el".text = ''
        	;; init.el --- -*- lexical-binding: t no-byte-compile: t -*-
        	;;; Commentary:
        	;;; Code:
        	(add-to-list 'load-path "~/.emacs.d/lisp")
        	
        	(require 'init-doom-modeline)
        	(require 'init-use-package)
        	(require 'init-dwim-shell-command)
        	(require 'init-perspective)
        	(require 'init-org-superstar)
        	(require 'init-flycheck)
        	(require 'init-em-banner)
        	(require 'init-corfu)
        	(require 'init-corfu-terminal)
        	(require 'init-custom)
        	(require 'init-docker)
        	(require 'init-org-modern)
        	(require 'init-cape)
        	(require 'init-keycast)
        	(require 'init-dashboard)
        	(require 'init-dired)
        	(require 'init-consult)
        	(require 'init-org-present)
        	(require 'init-ace-window)
        	(require 'init-savehist)
        	(require 'init-treesit)
        	(require 'init-marginalia)
        	(require 'init-move-text)
        	(require 'init-emacs)
        	(require 'init-vertico)
        	(require 'init-vertico-posframe)
        	(require 'init-orderless)
        	(require 'init-direnv)
        	(require 'init-nix-mode)
        	(require 'init-magit)
        	(require 'init-avy)
        	(require 'init-org-roam)
        	(require 'init-org)
        	(require 'init-yas)
        	(require 'init-salt-mode)
        	(require 'init-eglot)
        	(require 'init-custom-fun)
        	(require 'init-embark)
        	
        	(require 'tuxikus-eat)
        	;; init.el ends here
        '';
    
        ".emacs.d/lisp/init-ace-window.el".text = ''
    	    ;;; init-ace-window.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package ace-window
    	      :bind (("M-o" . ace-window))
    	      :config
    	      (setq aw-dispatch-always t)
    	      (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))
    	    
    	    (provide 'init-ace-window)
    	    
    	    ;;; init-ace-window.el ends here
      	'';
    
        ".emacs.d/lisp/init-avy.el".text = ''
        	;;; init-avy.el --- -*- lexical-binding: t -*-
        	;;; Commentary:
        	;;; Code:
        	
        	(use-package avy
        	  :bind
        	  (("M-g f" . avy-goto-line)
        	   ("M-g w" . avy-goto-word-1)
        	   ("C-'" . avy-goto-char-2)))
        	
        	(provide 'init-avy)
        	
        	;;; init-avy.el ends here
    	  '';
    
        ".emacs.d/lisp/init-cape.el".text = ''
    	    ;;; init-cape.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package cape
    	      :bind ("M-p" . cape-prefix-map)
    	      :init
    	      (add-hook 'completion-at-point-functions #'cape-dabbrev)
    	      (add-hook 'completion-at-point-functions #'cape-abbrev)
    	      (add-hook 'completion-at-point-functions #'cape-file)
    	      (add-hook 'completion-at-point-functions #'cape-elisp-block)
    	      (add-hook 'completion-at-point-functions #'cape-emoji)
    	      ;;(add-hook 'completion-at-point-functions #'cape-dict)
    	      (add-hook 'completion-at-point-functions #'cape-rfc1345)
    	      (add-hook 'completion-at-point-functions #'cape-sgml)
    	      (add-hook 'completion-at-point-functions #'cape-tex)
    	      (add-hook 'completion-at-point-functions #'cape-history))
    	    
    	    (provide 'init-cape)
    	    
    	    ;;; init-cape.el ends here
    	  '';
    
        ".emacs.d/lisp/init-consult.el".text = ''
    	    ;;; init-consult.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package consult
    	      :bind (;; C-c bindings in `mode-specific-map'
    	    	   ("C-c M-x" . consult-mode-command)
    	    	   ("C-c h" . consult-history)
    	    	   ("C-c k" . consult-kmacro)
    	    	   ("C-c m" . consult-man)
    	    	   ("C-c i" . consult-info)
    	    	   ([remap Info-search] . consult-info)
    	    	   ;; C-x bindings in `ctl-x-map'
    	    	   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
    	    	   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
    	    	   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    	    	   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
    	    	   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
    	    	   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
    	    	   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
    	    	   ;; Custom M-# bindings for fast register access
    	    	   ("M-#" . consult-register-load)
    	    	   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
    	    	   ("C-M-#" . consult-register)
    	    	   ;; Other custom bindings
    	    	   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
    	    	   ;; M-g bindings in `goto-map'
    	    	   ("M-g e" . consult-compile-error)
    	    	   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
    	    	   ("M-g g" . consult-goto-line)             ;; orig. goto-line
    	    	   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
    	    	   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
    	    	   ("M-g m" . consult-mark)
    	    	   ("M-g k" . consult-global-mark)
    	    	   ("M-g i" . consult-imenu)
    	    	   ("M-g I" . consult-imenu-multi)
    	    	   ;; M-s bindings in `search-map'
    	    	   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
    	    	   ("M-s c" . consult-locate)
    	    	   ("M-s g" . consult-grep)
    	    	   ("M-s G" . consult-git-grep)
    	    	   ("M-s r" . consult-ripgrep)
    	    	   ("M-s l" . consult-line)
    	    	   ("M-s L" . consult-line-multi)
    	    	   ("M-s k" . consult-keep-lines)
    	    	   ("M-s u" . consult-focus-lines)
    	    	   ;; Isearch integration
    	    	   ("M-s e" . consult-isearch-history)
    	    	   :map isearch-mode-map
    	    	   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
    	    	   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
    	    	   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
    	    	   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
    	    	   ;; Minibuffer history
    	    	   :map minibuffer-local-map
    	    	   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
    	    	   ("M-r" . consult-history)))                ;; orig. previous-matching-history-element
    	    
    	    (provide 'init-consult)
    	    
    	    ;;; init-consult.el ends here
    	  '';
    
        ".emacs.d/lisp/init-corfu.el".text = ''
    	    ;;; init-corfu.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package corfu
    	      :init
    	      (global-corfu-mode))
    	    
    	    (provide 'init-corfu)
    	    
    	    ;;; init-corfu.el ends here
    	  '';
    
        ".emacs.d/lisp/init-corfu-terminal.el".text = ''
    	    ;;; init-corfu-terminal.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package corfu
    	      :init
    	      (unless (display-graphic-p)
    	    (corfu-terminal-mode +1)))
    	    
    	    (provide 'init-corfu-terminal)
    	    
    	    ;;; init-corfu-terminal.el ends here
    	  '';
    
        ".emacs.d/lisp/init-custom-fun.el".text = ''
    	    ;;; init-custom-fun.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package custom-fun
    	      :load-path "~/.emacs.d/lisp/tuxikus"
    	      :demand)
    	    
    	    (provide 'init-custom-fun)
    	    
    	    ;;; init-custom-fun.el ends here
    	  '';
    
        ".emacs.d/lisp/init-dashboard.el".text = ''
    	    ;;; init-dashboard.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package dashboard
    	      :config
    	      (setq dashboard-projects-backend 'project-el)
    	    
    	      (setq dashboard-items '((recents   . 10)
    	    			    (bookmarks . 10)
    	    			    (projects  . 10)
    	    			    (agenda    . 10)
    	    			    (registers . 10)))
    	    
    	      (setq dashboard-item-shortcuts '((recents   . "r")
    	    				     (bookmarks . "m")
    	    				     (projects  . "p")
    	    				     (agenda    . "a")
    	    				     (registers . "e")))
    	    
    	      (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
    	    
    	      (dashboard-setup-startup-hook))
    	    
    	    (provide 'init-dashboard)
    	    
    	    ;;; init-dashboard.el ends here
    	  '';
    
        ".emacs.d/lisp/init-dired.el".text = ''
    	    ;;; init-dired.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package dired
    	      :config
    	      (put 'dired-find-alternate-file 'disabled nil)
    	      :hook
    	      (dired-mode . (lambda () (dired-hide-details-mode 1))))
    	    
    	    (provide 'init-dired)
    	    
    	    ;;; init-dired.el ends here
    	  '';
    
        ".emacs.d/lisp/init-direnv.el".text = ''
    	    ;;; init-direnv.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package direnv
    	      :config
    	      (direnv-mode))
    	    
    	    (provide 'init-direnv)
    	    
    	    ;;; init-direnv.el ends here
    	  '';
    
        ".emacs.d/lisp/init-docker.el".text = ''
        	;;; init-direnv.el --- -*- lexical-binding: t -*-
        	;;; Commentary:
        	;;; Code:
        	
        	(defcustom container-executable 'podman
        	  "The executable to be used with docker mode."
        	  :type '(choice
        		    (const :tag "docker" docker)
        		    (const :tag "podman" podman))
        	  :group 'custom)
        	
        	(use-package docker
        	  :bind
        	  ("C-c d" . docker)
        	  :config
        	  (pcase contaiter-executable
        	    ('docker
        	     (setf docker-command "docker"
        		     docker-compose-command "docker-compose"
        		     docker-container-tramp-method "docker"))
        	    ('podman
        	     (setf docker-command "podman"
        		     docker-compose-command "podman-compose"
        		     docker-container-tramp-methodu "podman"))))
        	
        	(provide 'init-docker)
        	
        	;;; init-docker.el ends here
    	  '';
    
        ".emacs.d/lisp/init-doom-modeline.el".text = ''
    	    ;;; init-doom-modeline.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package doom-modeline
    	      :init (doom-modeline-mode 1))
    	    
    	    (provide 'init-doom-modeline)
    	    
    	    ;;; init-doom-modeline.el ends here
    	  '';
    
        ".emacs.d/lisp/init-dwim-shell-command.el".text = ''
    	    ;;; init-dwim-shell-command.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package dwim-shell-command
    	      :config
    	      (unload-feature 'dwim-shell-command-autoloads t))
    	    
    	    (provide 'init-dwim-shell-command)
    	    
    	    ;;; init-dwim-shell-command.el ends here
    	  '';
    
        ".emacs.d/lisp/init-eglot.el".text = ''
    	    ;;; init-eglot.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package eglot
    	      :hook
    	      (add-hook 'python-ts-mode-hook 'eglot-ensure)
    	      (add-hook 'python-mode-hook 'eglot-ensure)
    	      :config
    	      :custom
    	      (eglot-autoshutdown t)  ;; shutdown language server after closing last file
    	      (eglot-confirm-server-initiated-edits nil))  ;; allow edits without confirmation
    	    
    	    (provide 'init-eglot)
    	    
    	    ;;; init-eglot.el ends here
    	  '';
    
        ".emacs.d/lisp/init-emacs.el".text = ''
    	      ;;; init-emacs.el --- -*- lexical-binding: t -*-
    	      ;;; Commentary:
    	      ;;; Code:
    	    
    	    (use-package emacs
    	      :bind
    	      ("M-<tab>" . completion-at-point)
    	      ;;("" . duplicate-line)
    	    
    	      :init
    	      (setq create-lockfiles nil
    	    	make-backup-files nil
    	    	custom-theme-directory "~/.emacs.d/themes"
    	    	inhibit-startup-message t
    	    	inhibit-startup-screen t
    	    	initial-scratch-message ";;; Emacs is fun"
    	    	global-auto-revert-non-file-buffers t)
    	      (fset 'yes-or-no-p 'y-or-n-p)
    	      (tool-bar-mode -1)
    	      (menu-bar-mode -1)
    	      (scroll-bar-mode -1)
    	      (save-place-mode 1)
    	      (global-auto-revert-mode 1)
    	      (load-theme 'doom-bluloco-light t)
    	      (set-face-attribute 'default nil
    	    			:family "Iosevka Nerd Font"
    	    			:height ${config.fontSize}
    	    			:weight 'regular
    	    			:width 'normal)
    	    
    	      (set-face-attribute 'bold nil
    	    		      :family "Iosevka Nerd Font"
    	    		      :weight 'regular)
    	    
    	      (set-face-attribute 'italic nil
    	    		      :family "Iosevka Nerd Font"
    	    		      :slant 'italic
    	    		      :weight 'regular)
    	    
    	      (set-face-attribute 'bold-italic nil
    	    		      :family "Iosevka Nerd Font"
    	    		      :weight 'regular
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
    	      :config
    	      (electric-pair-mode)
    	      :custom
    	      (enable-recursive-minibuffers t)
    	      (read-extended-command-predicate #'command-completion-default-include-p)
    	    
    	      ;; (tab-always-indent 'complete)
    	    
    	      ;; Emacs 30 and newer: Disable Ispell completion function.
    	      ;; Try `cape-dict' as an alternative.
    	      (text-mode-ispell-word-completion nil)
    	    
    	      ;; Hide commands in M-x which do not apply to the current mode.  Corfu
    	      ;; commands are hidden, since they are not used via M-x. This setting is
    	      ;; useful beyond Corfu.
    	      (read-extended-command-predicate #'command-completion-default-include-p))
    	    
    	    (provide 'init-emacs)
    	    
    	      ;;; init-emacs.el ends here
    	  '';
    
        ".emacs.d/lisp/init-embark.el".text = ''
    	    ;;; init-embark.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package embark
    	      :bind
    	      ("C-." . embark-act)
    	      ("M-." . embark-dwim))
    	    
    	    (provide 'init-embark)
    	    
    	    ;;; init-embark.el ends here
    	  '';
    
        ".emacs.d/lisp/init-em-banner.el".text = ''
    	    ;;; init-em-banner.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package em-banner)
    	    
    	    (provide 'init-em-banner)
    	    
    	    ;;; init-em-banner.el ends here
    	  '';
    
        ".emacs.d/lisp/init-flycheck.el".text = ''
    	    ;;; init-flycheck.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package flycheck
    	      :hook
    	      (after-init . global-flycheck-mode))
    	    
    	    (provide 'init-flycheck)
    	    
    	    ;;; init-flycheck.el ends here
    	  '';
    
        ".emacs.d/lisp/init-keycast.el".text = ''
    	    ;;; init-keycast.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package keycast
    	      :config
    	      (keycast-header-line-mode))
    	    
    	    (provide 'init-keycast)
    	    
    	    ;;; init-keycast.el ends here
    	  '';
    
        ".emacs.d/lisp/init-magit.el".text = ''
    	    ;;; init-magit.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package magit)
    	    
    	    (provide 'init-magit)
    	    
    	    ;;; init-magit.el ends here
    	  '';
    
        ".emacs.d/lisp/init-marginalia.el".text = ''
    	    ;;; init-marginalia.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package marginalia
    	      :bind (:map minibuffer-local-map
    	    	   ("M-A" . marginalia-cycle))
    	      :init
    	      (marginalia-mode))
    	    
    	    (provide 'init-marginalia)
    	    
    	    ;;; init-marginalia.el ends here
    	  '';
    
        ".emacs.d/lisp/init-move-text.el".text = ''
    	    ;;; init-move-text.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package move-text
    	      :config
    	      (move-text-default-bindings))
    	    
    	    (provide 'init-move-text)
    	    
    	    ;;; init-move-text.el ends here
    	    
    	  '';
    
        ".emacs.d/lisp/init-nix-mode.el".text = ''
    	    ;;; init-nix-mode.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package nix-mode
    	      :mode "\\.nix\\'")
    	    
    	    (provide 'init-nix-mode)
    	    
    	    ;;; init-nix-mode.el ends here
    	  '';
    
        ".emacs.d/lisp/init-orderless.el".text = ''
    	    ;;; init-orderless.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package orderless
    	      :custom
    	      (completion-styles '(orderless flex))
    	      (completion-category-defaults nil)
    	      (completion-category-overrides '((file (styles basic partial-completion)))))
    	    
    	    (provide 'init-orderless)
    	    
    	    ;;; init-orderless.el ends here
    	  '';
    
        ".emacs.d/lisp/init-org.el".text = ''
    	    ;;; init-org.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package org
    	      :init
    	      (setq org-attach-id-dir "~/org/.attach"
    	    	org-log-done 'time
    	    	org-hide-emphasis-markers t
    	    	org-imenu-depth 7)
    	    
    	      :config
    	      (set-face-attribute 'org-level-1 nil :height 1.5)
    	      (set-face-attribute 'org-level-2 nil :height 1.4)
    	      (set-face-attribute 'org-level-3 nil :height 1.3)
    	      (set-face-attribute 'org-level-4 nil :height 1.2)
    	      (set-face-attribute 'org-level-5 nil :height 1.1)
    	      (set-face-attribute 'org-level-6 nil :height 1.0)
    	      (set-face-attribute 'org-level-7 nil :height 1.0)
    	      (set-face-attribute 'org-level-8 nil :height 1.0)
    	      (set-face-attribute 'org-block-begin-line nil :background "#f0f0f0")
    	      (set-face-attribute 'org-block-end-line nil :background "#f0f0f0")
    	      (set-face-attribute 'org-document-title nil :height 2.0)
    	    
    	      ;; load org babel languages
    	      (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
    	    							   (emacs-lisp . t)
    	    							   (python . t)))
    	      :bind
    	      ("C-M-<return>" . org-insert-subheading))
    	    
    	    (provide 'init-org)
    	    
    	    ;;; init-org.el ends here
    	  '';
    
        ".emacs.d/lisp/init-org-roam.el".text = ''
    	    ;;; init-org-roam.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package org-roam
    	      :custom
    	      (org-roam-directory (concat org-directory "/roam"))
    	      :config
    	      ;; If you're using a vertical completion framework, you might want a more informative completion interface
    	      ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    	      (org-roam-db-autosync-mode)
    	      ;; If using org-roam-protocol
    	      (require 'org-roam-protocol))
    	    
    	    (provide 'init-org-roam)
    	    
    	    ;;; init-org-roam.el ends here
    	  '';
    
        ".emacs.d/lisp/init-org-modern.el".text = ''
    	    ;;; init-org-modern.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package org-modern
    	      :config
    	      (with-eval-after-load 'org (global-org-modern-mode)))
    	    
    	    (provide 'init-org-modern)
    	    
    	    ;;; init-org-modern.el ends here
    	    
    	  '';
    
        ".emacs.d/lisp/init-org-superstar.el".text = ''
    	    ;;; init-org-superstar.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package org-superstar
    	      :hook
    	      (org-mode . (lambda () (org-superstar-mode 1))))
    	    
    	    (provide 'init-org-superstar)
    	    
    	    ;;; init-org-superstar.el ends here
    	  '';
    
        ".emacs.d/lisp/init-org-present.el".text = ''
    	    ;;; init-org-present.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package org-present)
    	    
    	    (provide 'init-org-present)
    	    
    	    ;;; init-org-present.el ends here
    	  '';
    
        ".emacs.d/lisp/init-perspective.el".text = ''
    	    ;;; init-perspective.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package perspective
    	      :bind
    	      ("C-x x s" . persp-switch)
    	      ("C-x x x" . persp-kill)
    	      :init
    	      (persp-mode))
    	    
    	    (provide 'init-perspective)
    	    
    	    ;;; init-perspective.el ends here
    	  '';
    
        ".emacs.d/lisp/init-salt-mode.el".text = ''
    	    ;;; init-salt-mode.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package salt-mode
    	      :hook
    	      (salt-mode . (lambda () (flyspell-mode 1))))
    	    
    	    (provide 'init-salt-mode)
    	    
    	    ;;; init-salt-mode.el ends here
    	  '';
    
        ".emacs.d/lisp/init-savehist.el".text = ''
    	    ;;; init-savehist.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package savehist
    	      :init
    	      (savehist-mode))
    	    
    	    (provide 'init-savehist)
    	    
    	    ;;; init-savehist.el ends here
    	  '';
    
        ".emacs.d/lisp/init-treesit.el".text = ''
    	    ;;; init-treesit.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package treesit
    	      :init
    	      (setq major-mode-remap-alist
    	    	  '((bash-mode . bash-ts-mode)
    	    	    (python-mode . python-ts-mode))))
    	    
    	    (provide 'init-treesit)
    	    
    	    ;;; init-treesit.el ends here
    	  '';
    
        ".emacs.d/lisp/init-use-package.el".text = ''
    	    ;;; init-use-package.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package use-package
    	      :config
    	      (setq use-package-compute-statistics t))
    	    
    	    (provide 'init-use-package)
    	    
    	    ;;; init-use-package.el ends here
    	  '';
    
        ".emacs.d/lisp/init-vertico.el".text = ''
    	    ;;; init-vertico.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package vertico
    	      :custom
    	      (vertico-scroll-margin 0) ;; Different scroll margin
    	      (vertico-count 20) ;; Show more candidates
    	      ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
    	      (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    	      :init
    	      (vertico-mode))
    	    
    	    (provide 'init-vertico)
    	    
    	    ;;; init-vertico.el ends here
    	  '';
    
        ".emacs.d/lisp/init-vertico-posframe.el".text = ''
    	    ;;; init-vertico-posframe.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package vertico-posframe
    	      :init
    	      (setq vertico-multiform-commands
    	    	'((consult-line (:not posframe))
    	    	  (t posframe)))
    	      
    	      (vertico-posframe-mode 1))
    	    
    	    (provide 'init-vertico-posframe)
    	    
    	    ;;; init-vertico-posframe.el ends here
    	  '';
    
        ".emacs.d/lisp/init-yas.el".text = ''
    	    ;;; init-yas.el --- -*- lexical-binding: t -*-
    	    ;;; Commentary:
    	    ;;; Code:
    	    
    	    (use-package yasnippet
    	      :config
    	      (yas-global-mode 1))
    	    
    	    (provide 'init-yas)
    	    
    	    ;;; init-yas.el ends here
    	  '';
    
        # custom init
        ".emacs.d/lisp/init-custom.el".text = ''
    	    ${config.customInit}
    
      	  (provide 'init-custom)
    	  '';
    
        # custom functions
        ".emacs.d/lisp/tuxikus/custom-fun.el".text = ''
        	(defun tuxikus/get-jira-ticket-number (branch)
        	  (when (string-match "[A-Z]\\{8\\}-[0-9]*" branch)
        	    (message (match-string 0 branch))))
        	
        	(add-hook 'git-commit-setup-hook '(lambda () (insert (tuxikus/get-jira-ticket-number (magit-get-current-branch)))))
        	
        	(defun tuxikus/get-bookmarks-from-file ()
        	  "Get bookmarks from the bookmark file"
        	  (with-temp-buffer
        	    (insert-file-contents "~/.bookmarks.org")
        	    (org-mode)
        	    (let (bookmarks)
        	      (org-element-map (org-element-parse-buffer) 'link
        		(lambda (l)
        		  (let* ((link (org-element-property :raw-link l))
        			 (name (org-element-interpret-data (org-element-contents l)))
        			 (tags (org-element-property :tags (org-element-property :parent l))))
        		    (push (concat name
        				  "\n"
        				  link
        				  "\n"
        				  (format "[%s]" (mapconcat #'identity tags ", "))) bookmarks))))
        	      bookmarks)))
        	
        	(defun tuxikus/add-bookmark ()
        	  "Add a new bookmark to the bookmark file."
        	  (interactive)
        	  (let* ((title (read-from-minibuffer "Title: "))
        		 (url (read-from-minibuffer "URL: "))
        		 (tags (read-from-minibuffer "Tags: ")))
        	    (write-region (format "* [[%s][%s]] %s\n" url title tags) nil "~/.bookmarks.org" 'append)))
        	
        	(defun tuxikus/edit-bookmark ()
        	  "TODO implement."
        	  (interactive)
        	  (message "Not implemented."))
        	
        	(defun tuxikus/delete-bookmark ()
        	  "TODO implement."
        	  (interactive)
        	  (message "Not implemented."))
        	
        	(defun tuxikus/open-bookmark ()
        	  "Select a bookmark and open it."
        	  (interactive)
        	  (browse-url
        	   (seq-elt (split-string
        		     (completing-read "Open: " (tuxikus/get-bookmarks-from-file))
        		     "\n") 1)))
        	
        	(defun tuxikus/change-org-directory ()
        	  "Change the active org directory."
        	  (interactive)
        	  (let ((selection (completing-read "Select: " '("~/org" "~/org-edu"))))
        	    (setq org-directory selection
        		  org-attach-id-dir (concat org-directory "/.attach")
        		  org-roam-directory (concat org-directory "/roam")
        		  org-roam-db-location (concat org-directory "/org-roam.db"))))
        	
        	(provide 'custom-fun)
        '';
    
        # themes
        ".emacs.d/themes/tuxikus-basic-theme".text = ''
        	(deftheme tuxikus-basic
        	  "Nice theme")
        	
        	(custom-theme-set-faces
        	 'tuxikus-basic
        	 '(default ((t (:family "Iosevka Nerd Font" :width normal :height 151 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#00ff00" :background "#000000" :stipple nil :inherit nil))))
        	 '(cursor ((t (:background "#ffffff"))))
        	 '(fixed-pitch ((t (:family "Monospace"))))
        	 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
        	 '(escape-glyph ((t (:foreground "#e7a59a"))))
        	 '(homoglyph ((t (:foreground "#f5aa80"))))
        	 '(minibuffer-prompt ((t (:inherit (modus-themes-prompt)))))
        	 '(highlight ((t (:foreground "#00ff00" :background "#00415e"))))
        	 '(region ((t (:extend t :foreground "#ffffff" :background "#3c3c3c"))))
        	 '(shadow ((t (:foreground "#a8a8a8"))))
        	 '(secondary-selection ((t (:extend t :inherit (modus-themes-special-cold)))))
        	 '(trailing-whitespace ((t (:background "#a4202a"))))
        	 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
        	 '(font-lock-builtin-face ((t (:foreground "#f78fe7" :inherit (modus-themes-bold)))))
        	 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
        	 '(font-lock-comment-face ((t (:foreground "#a8a8a8" :inherit (modus-themes-slant)))))
        	 '(font-lock-constant-face ((t (:foreground "#00bcff"))))
        	 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
        	 '(font-lock-doc-face ((t (:foreground "#b0d6f5" :inherit (modus-themes-slant)))))
        	 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
        	 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
        	 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
        	 '(font-lock-function-name-face ((t (:foreground "#feacd0"))))
        	 '(font-lock-keyword-face ((t (:foreground "#b6a0ff" :inherit (modus-themes-bold)))))
        	 '(font-lock-negation-char-face ((t (:foreground "#d0bc00" :inherit (modus-themes-bold)))))
        	 '(font-lock-number-face ((t nil)))
        	 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
        	 '(font-lock-operator-face ((t nil)))
        	 '(font-lock-preprocessor-face ((t (:foreground "#ff9077"))))
        	 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
        	 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
        	 '(font-lock-punctuation-face ((t nil)))
        	 '(font-lock-regexp-grouping-backslash ((t (:foreground "#abab00" :inherit (modus-themes-bold)))))
        	 '(font-lock-regexp-grouping-construct ((t (:foreground "#e7a59a" :inherit (modus-themes-bold)))))
        	 '(font-lock-string-face ((t (:foreground "#79a8ff"))))
        	 '(font-lock-type-face ((t (:foreground "#6ae4b9" :inherit (modus-themes-bold)))))
        	 '(font-lock-variable-name-face ((t (:foreground "#00d3d0"))))
        	 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
        	 '(font-lock-warning-face ((t (:foreground "#d0bc00" :inherit (modus-themes-bold)))))
        	 '(button ((t (:underline (:color foreground-color :style line :position nil) :foreground "#00bcff"))))
        	 '(link ((t (:inherit (button)))))
        	 '(link-visited ((t (:underline (:color foreground-color :style line :position nil) :foreground "#b6a0ff" :inherit (button)))))
        	 '(fringe ((t (:foreground "#ffffff" :background "#000000"))))
        	 '(header-line ((t (:box (:line-width (4 . 4) :color "#212121" :style nil) :foreground "#dddddd" :background "#212121" :inherit (modus-themes-ui-variable-pitch)))))
        	 '(tooltip ((t (:foreground "#ffffff" :background "#203448"))))
        	 '(mode-line ((t (:box (:line-width (6 . 6) :color "#2a2a66" :style nil) :foreground "#ffffff" :background "#2a2a66" :inherit (modus-themes-ui-variable-pitch)))))
        	 '(mode-line-buffer-id ((t (:inherit (bold)))))
        	 '(mode-line-emphasis ((t (:foreground "#d5b1ff" :inherit (bold)))))
        	 '(mode-line-highlight ((t (:box (:line-width (1 . 1) :color "#ffffff" :style nil) :inherit (highlight)))))
        	 '(mode-line-inactive ((t (:box (:line-width (6 . 6) :color "#1e1e1e" :style nil) :foreground "#bfc0c4" :background "#1e1e1e" :inherit (modus-themes-ui-variable-pitch)))))
        	 '(isearch ((t (:inherit (modus-themes-search-success)))))
        	 '(isearch-fail ((t (:inherit (modus-themes-refine-red)))))
        	 '(lazy-highlight ((t (:inherit (modus-themes-search-success-lazy)))))
        	 '(match ((t (:inherit (modus-themes-special-calm)))))
        	 '(next-error ((t (:extend t :inherit (modus-themes-subtle-red)))))
        	 '(query-replace ((t (:inherit (modus-themes-intense-red))))))
        	
        	(provide-theme 'tuxikus-basic)
    	  '';
      };
    };
  };
}

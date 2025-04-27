{
  config,
  pkgs,
  lib,
  ...
}:
let
  my-emacs = config.emacsPkg.override {
    withNativeCompilation = true;
  };
  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (
    epkgs: with epkgs; [
      aas
      ace-window
      avy
      cape
      consult
      consult-yasnippet
      corfu
      direnv
      dirvish
      docker
      dockerfile-mode
      dslide
      eat
      embark
      embark-consult
      embark-org-roam
      ess
      exec-path-from-shell
      exwm
      fireplace
      flycheck
      flycheck-inline
      format-all
      git-link
      go-mode
      hide-mode-line
      htmlize
      hydra
      ledger-mode
      magit
      marginalia
      move-text
      nix-mode
      orderless
      org-download
      org-modern
      org-roam
      org-roam-ui
      org-superstar
      pass
      pdf-tools
      python-mode
      pyvenv
      ripgrep
      rust-mode
      salt-mode
      spacious-padding
      tabspaces
      verb
      vertico
      vterm
      vundo
      walkman
      wgrep
      yasnippet
      (trivialBuild {
        pname = "moc";
        version = "v0.6.2";
        src = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/positron-solutions/moc/refs/heads/master/moc.el";
          sha256 = "sha256-rwsfM+FvWb0sviT2TtCVlWW8rfW6XBHlch4AbvhaL00=";
        };

        nativeBuildInputs = [ hide-mode-line ];
      })
      (treesit-grammars.with-grammars (
        grammars: with grammars; [
          tree-sitter-python
          tree-sitter-bash
          tree-sitter-c
          tree-sitter-go
          tree-sitter-gomod
          tree-sitter-rust
        ]
      ))
    ]
  );
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

    home.file.".emacs.d/init.el".text = ''
      ;;; init.el --- Emacs configuration file
      ;;; Commentary:
      ;;; Code:
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                                            custom                                            ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (defcustom container-executable 'podman
        "The executable to be used with docker mode."
        :type '(choice
                (const :tag "docker" docker)
                (const :tag "podman" podman))
        :group 'custom)
      
      (defcustom tuxikus/ssh-config-file "~/.ssh/config"
        "SSH config file path."
        :type '(string)
        :group 'custom)
      
      (defcustom tuxikus/nix-config-directory "~/projects/personal/nix-config/"
        "Nix config directory."
        :type '(string)
        :group 'custom)
      
      (defcustom tuxikus/nix-flake-host "zeus"
        "Nix flake host."
        :type '(string)
        :group 'custom)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                                          use-package                                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                          aas                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package aas
        :config
        (aas-set-snippets 'text-mode
          "o:" "ö"
          "O:" "Ö"
          "u:" "ü"
          "U:" "Ü"
          "a:" "ä"
          "A:" "Ä"
          "sz" "ß")
        :hook
        ((LaTeX-mode . aas-activate-for-major-mode)
         (org-mode . aas-activate-for-major-mode)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      ace-window                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package ace-window
        :bind
        (("M-o" . ace-window))
        :custom
        (aw-dispatch-always t)
        (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                          avy                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package avy
        :bind
        (("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("C-'" . avy-goto-char-2)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         cape                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package cape
        :bind ("M-p" . cape-prefix-map))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        consult                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package consult
        :bind
        (("C-c M-x" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                   consult-yasnippet                  ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package consult-yasnippet)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         corfu                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package corfu
        :custom
        (corfu-auto nil)
        (corfu-echo-documentation nil)
        (tab-always-indent 'complete)
        (completion-cycle-threshold nil)
        :hook
        (after-init . global-corfu-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         dired                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dired
        :config
        (setq dired-listing-switches
              "-l --almost-all --human-readable --group-directories-first --no-group"))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        direnv                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package direnv
        :bind
        (("C-c f" . dirvish)
         :map dirvish-mode-map
         ("o" . dirvish-quick-access))
        :custom
        (dirvish-quick-access-entries
         '(("h" "~/"                          "Home")
           ("d" "~/Downloads/"                "Downloads")
           ("m" "/mnt/"                       "Drives")))
        :hook
        (after-init . direnv-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        dirvish                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dirvish
        :hook
        (after-init . dirvish-override-dired-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        docker                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package docker
        :bind
        ;;("C-c d" . docker)
        :config
        (pcase container-executable
          ('docker
           (setf docker-command "docker"
                 docker-compose-command "docker-compose"
                 docker-container-tramp-method "docker"))
          ('podman
           (setf docker-command "podman"
                 docker-compose-command "podman-compose"
                 docker-container-tramp-method "podman"))))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                    dockerfile-mode                   ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dockerfile-mode
        :mode "Dockerfile\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        dslide                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dslide)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                          eat                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package eat
        :bind
        (("C-c t e". eat)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         eglot                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package eglot
        ;; :hook
        ;; ((python-ts-mode . eglot-ensure)
        ;;  (python-mode . eglot-ensure))
        :custom
        (eglot-autoshutdown t)
        (eglot-confirm-server-initiated-edits nil))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       electric                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package electric
        :init
        ;;(setq electric-pair-preserve-balance nil)
        (electric-pair-mode)
        :config
        (defvar latex-mode-electric-pairs '((?$ . ?$))
          "Electric pairs for LaTeX mode.")
        (defvar org-mode-electric-pairs '(())
          "Electric pairs for org mode.")
        (defun latex-mode-add-electric-pairs ()
          "Add electric pairs for LaTeX mode."
          (setq-local electric-pair-pairs (append electric-pair-pairs latex-mode-electric-pairs))
          (setq-local electric-pair-text-pairs electric-pair-pairs)
          (message "Electric pairs added for LaTeX mode: %s" electric-pair-pairs))
        (defun org-mode-add-electric-pairs ()
          "Add electric pairs for org mode."
          (setq-local electric-pair-pairs (append electric-pair-pairs
                                                  org-mode-electric-pairs
                                                  latex-mode-electric-pairs))
          (setq-local electric-pair-text-pairs electric-pair-pairs)
          (message "Electric pairs added for org mode: %s" electric-pair-pairs))
        :hook
        (latex-mode . latex-mode-add-electric-pairs)
        (org-mode . org-mode-add-electric-pairs))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       em-banner                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package em-banner)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         emacs                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package emacs
        :bind
        (("M-<tab>" . completion-at-point)
         ("C-c d" . duplicate-line)
         ("C-c e r" . eval-region)
         ("C-c e b" . eval-buffer)
         ("C-c w m" . whitespace-mode)
         ("C-x c" . compile)
         ("C-S-a" . beginning-of-buffer)
         ("C-S-e" . end-of-buffer)
         ("M-z" . zap-up-to-char)
         ("C-z" . nil))
        :hook
        ((before-save . whitespace-cleanup)
         (makefile-mode . indent-tabs-mode)
         (prog-mode . display-line-numbers-mode)
         (git-commit-setup . tuxikus/insert-jira-ticket-number))
      
        :init
        (fset 'yes-or-no-p 'y-or-n-p)
        (auto-save-mode -1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        ;;(save-place-mode 1)
        (global-auto-revert-mode 1)
        (setq-default indent-tabs-mode nil)
        (setq ring-bell-function 'ignore)
        (setq display-line-numbers-type 'relative)
        (load-theme 'modus-operandi t)
        :config
        ;; mode line
        (setq-default mode-line-format
                      (list
                       '(:eval (propertize (format " %s " (buffer-name))
                                           'face '(:weight bold)))
                       '(:eval (propertize (format " %s " "|")
                                           'face '(:weight bold)))
                       '(:eval (propertize (format " %s " major-mode)
                                           'face '(:weight bold)))))
        ;; move mode line to top
        (setq-default header-line-format mode-line-format)
        (setq-default mode-line-format nil)
        ;; tab bar
        (setq tab-bar-new-button nil
              tab-bar-close-button nil)
        ;; Customize the appearance of the tab-bar
        (set-face-attribute 'tab-bar nil
                            :height 0.9  ;; Adjust the height of the tab text
                            :weight 'bold)  ;; Make the tab text bold
        (setq create-lockfiles nil
              make-backup-files nil
              custom-theme-directory "~/.emacs.d/themes"
              inhibit-startup-message t
              inhibit-startup-screen t
              initial-scratch-message ";;; Emacs is fun"
              global-auto-revert-non-file-buffers t
              org-id-uuid-program "~/.local/bin/uuidgenlc")
        ;; (set-frame-font "Iosevka Nerd Font-15" nil t) ; test fonts
        (add-to-list 'default-frame-alist
                     '(font . "Iosevka Nerd Font-${config.fontSize}"))
        (which-key-mode 1)
        :custom
        (enable-recursive-minibuffers t)
        (read-extended-command-predicate #'command-completion-default-include-p)
        ;; Emacs 30 and newer: Disable Ispell completion function.
        ;; Try `cape-dict' as an alternative.
        (text-mode-ispell-word-completion nil)
        ;; Hide commands in M-x which do not apply to the current mode.
        (read-extended-command-predicate #'command-completion-default-include-p))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        embark                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package embark
        :bind
        ("C-." . embark-act)
        ("M-." . embark-dwim))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                    embark-consult                    ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package embark-consult)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                    embark-org-roam                   ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package embark-org-roam)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                          ess                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package ess)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                 exec-path-from-shell                 ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package exec-path-from-shell
        :config
        (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize))
        (when (daemonp)
          (exec-path-from-shell-initialize)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         exwm                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package exwm
        :if (eq window-system 'x)
        :init
        (setq exwm-workspace-number 9)
        ;; Make class name the buffer name.
        (add-hook 'exwm-update-class-hook
                  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
        ;; Global keybindings.
        (setq exwm-input-global-keys
              `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
                ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
                ([?\s-d] . (lambda (cmd) ;; s-d: Launch application.
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command cmd nil cmd)))
                ;; s-N: Switch to certain workspace.
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9))))
        ;; Enable EXWM
        (exwm-enable))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         hydra                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package hydra
        :bind
        (("C-c h o" . tuxikus/org-hydra/body)
         ("C-c h n" . tuxikus/nix-hydra/body)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       fireplace                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package fireplace)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       flycheck                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package flycheck
        :hook
        (after-init . global-flycheck-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                    flycheck-inline                   ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package flycheck-inline
        :config
        (with-eval-after-load 'flycheck
          (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      format-all                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package format-all)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       git-link                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package git-link)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        go-mode                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package go-mode
        :mode "\\.go\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                    hide-mode-line                    ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package hide-mode-line)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        htmlize                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package htmlize)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      ledger-mode                     ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package ledger-mode
        :mode "\\.lgr\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         magit                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package magit
        :bind
        ("C-x g" . magit))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      marginalia                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package marginalia
        :bind (:map minibuffer-local-map
                    ("M-A" . marginalia-cycle))
        :init
        (marginalia-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                          moc                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package moc)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       move-text                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package move-text
        :init
        (move-text-default-bindings))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       nix-mode                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package nix-mode
        :mode "\\.nix\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       orderless                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package orderless
        :custom
        (completion-styles '(orderless flex))
        (completion-category-defaults nil)
        (completion-category-overrides '((file (styles basic partial-completion)))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                          org                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org
        :bind
        (("C-M-<return>" . org-insert-subheading))
        :custom
        ((org-attach-id-dir "~/org/.attach")
         (org-log-done 'time)
         (org-imenu-depth 7)
         (org-complete-tags-always-offer-all-agenda-tags t))
        :init
        (setq org-todo-keywords
              '((sequence "TODO(t)" "|" "DONE(D)" "CANCEL(C)")
                (sequence "MEET(m)" "|" "MET(M)")
                (sequence "STUDY(s)" "|" "STUDIED(S)")
                (sequence "WRITE(w)" "|" "WROTE(W)")))
      
        (setq org-todo-keyword-faces
              '(("MEET" . (:inherit (bold org-todo)))
                ("STUDY" . (:inherit (warning org-todo)))
                ("WRITE" . (:inherit (shadow org-todo)))))
        (org-babel-do-load-languages
         'org-babel-load-languages '((shell . t)
                                     (emacs-lisp . t)
                                     (python . t)
                                     (R . t)
                                     (dot . t)
                                     (gnuplot . t))))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                     org-download                     ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-download
        :init
        (setq org-download-method 'attach))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      org-modern                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-modern
        :defer t
        :hook
        (org-mode . org-modern-mode)
        (org-agenda-finalize . org-modern-agenda))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       org-roam                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-roam
        :bind
        (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert))
        :custom
        (org-roam-directory (concat org-directory "/roam"))
        (org-roam-dailies-capture-templates
         '(("d" "default" entry "* %<%H:%M %p>: %?"
            :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
        :config
        ;; If you're using a vertical completion framework, you might want a more informative completion interface
        (setq org-roam-node-display-template (concat "''${title:*} " (propertize "''${tags:10}" 'face 'org-tag)))
        (org-roam-db-autosync-mode)
        (setq org-roam-completion-everywhere t)
        ;; If using org-roam-protocol
        (require 'org-roam-protocol))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      org-roam-ui                     ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-roam-ui)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                     org-superstar                    ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-superstar
        :hook
        (org-mode . org-superstar-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         pass                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package pass)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       pdf-tools                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package pdf-tools
        :init
        (pdf-tools-install))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                      python-mode                     ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package python-mode
        :defer t
        :mode "\\.py\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        pyvenv                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package pyvenv)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        ripgrep                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package ripgrep)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       rust-mode                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package rust-mode
        :defer t
        :mode "\\.rs\\'")
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       salt-mode                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package salt-mode
        :defer t
        :mode "\\.sls\\'")
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       savehist                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package savehist
        :init
        (savehist-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                   spacious-padding                   ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package spacious-padding
        :init
        (setq spacious-padding-widths
              '( :internal-border-width 10
                 :header-line-width 5
                 :mode-line-width 5
                 :tab-width 10
                 :right-divider-width 30
                 :scroll-bar-width 8
                 :fringe-width 15))
        ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
        ;; is very flexible and provides several examples.
        (setq spacious-padding-subtle-mode-line
              `( :mode-line-active 'default
                 :mode-line-inactive vertical-border))
        (spacious-padding-mode 1))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       tabspaces                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package tabspaces
        :hook
        (after-init . tabspaces-mode)
        :commands
        (tabspaces-switch-or-create-workspace
         tabspaces-open-or-create-project-and-workspace)
        :custom
        (tabspaces-use-filtered-buffers-as-default t)
        (tabspaces-default-tab "Default")
        (tabspaces-remove-to-default t)
        (tabspaces-include-buffers '("*scratch*"))
        (tabspaces-initialize-project-with-todo t)
        (tabspaces-todo-file-name "project-todo.org")
        ;; sessions
        (tabspaces-session t)
        (tabspaces-session-auto-restore t)
        (tab-bar-new-tab-choice "*scratch*")
        :config
        (with-eval-after-load 'consult
          ;; hide full buffer list (still available with "b" prefix)
          (consult-customize consult--source-buffer :hidden t :default nil)
          ;; set consult-workspace buffer list
          (defvar consult--source-workspace
            (list :name     "Workspace Buffers"
                  :narrow   ?w
                  :history  'buffer-name-history
                  :category 'buffer
                  :state    #'consult--buffer-state
                  :default  t
                  :items    (lambda () (consult--buffer-query
                                        :predicate #'tabspaces--local-buffer-p
                                        :sort 'visibility
                                        :as #'buffer-name)))
            "Set workspace buffer list for consult-buffer.")
          (add-to-list 'consult-buffer-sources 'consult--source-workspace)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        treesit                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package treesit
        :init
        (setq major-mode-remap-alist
              '((bash-mode . bash-ts-mode)
                (python-mode . python-ts-mode)
                (go-mode . go-ts-mode)
                (rust-mode . rust-ts-mode))))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         verb                         ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package verb)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        vertico                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package vertico
        :custom
        (vertico-scroll-margin 0)
        (vertico-count 10)
        (vertico-cycle t)
        :init
        (vertico-mode))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         vterm                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package vterm
        :bind
        (("C-c t v" . vterm)))
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         vundo                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package vundo)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                        walkman                       ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package walkman)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                         wgrep                        ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package wgrep)
      
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;                       yasnippet                      ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package yasnippet
        :init
        (yas-global-mode 1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            hydras                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (defhydra tuxikus/org-hydra (:color green :hint nil)
        "Org hydra"
        ;; Roam
        ("rf" org-roam-node-find "Roam node find" :column "Roam")
        ("ri" org-roam-node-insert "Roam node insert" :column "Roam")
        ("rc" tuxikus/change-org-directory "Change org directory" :column "Roam")
        ("rc" org-roam-dailies-capture-today "Capture daily" :column "Roam")
        ;; Clock
        ("ci" org-clock-in "Clock in" :column "Clock")
        ("co" org-clock-out "Clock out" :column "Clock")
        ("q" nil "quit" :column "Options"))
      
      (defhydra tuxikus/nix-hydra (:color green :hint nil)
        "Nix hydra"
        ("u" tuxikus/nix-flake-update "Nix flake update")
        ("r" tuxikus/nix-rebuild-switch "Nix flake update")
        ("q" nil "quit"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                           functions                                          ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (defun tuxikus/get-jira-ticket-number (branch)
        (when (string-match "[A-Z]\\{8\\}-[0-9]*" branch)
          (message (match-string 0 branch))))
      
      (defun tuxikus/insert-jira-ticket-number ()
        (insert (concat (tuxikus/get-jira-ticket-number (magit-get-current-branch) ": "))))
      
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
                                (format "[%s]" (mapconcat #'identity tags ", "))) bookmarks)))) bookmarks)))
      
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
        "Select a bookmark and open it in the default browser"
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
      
      (defun tuxikus/delete-current-file ()
        (interactive)
        (let ((file (buffer-file-name)))
          (when file
            (progn
              (delete-file file)
              (kill-buffer)
              (message "%s deleted" file)))))
      
      (defun tuxikus/org-set-sha256sum ()
        "Insert the sha256sum of the attachment at point."
        (interactive)
        (org-set-property
         "sha256"
         (concat
          "  "
          (string-trim (car (split-string
                             (shell-command-to-string
                              (format "sha256sum %s" (tuxikus/org-attach-id-get-path))) " "))))))
      
      (defun tuxikus/org-get-property (property)
        "Return the property PROPERTY of the org heading at point."
        (interactive "sProperty: ")
        (let ((property-value (org-entry-get (point) property)))
          (if property-value
              property-value
            nil)))
      
      (defun tuxikus/org-attach-id-get-path ()
        "Return the path of the attachment at point."
        (interactive)
        (let* ((attachment-dir (tuxikus/org-get-property "id"))
               (first-part (substring attachment-dir 0 2))
               (second-part (substring attachment-dir 2))
               (final-dir (concat org-attach-id-dir "/" first-part "/" second-part))
               (files (directory-files final-dir))
               (files (remove "." files))
               (files (remove ".." files))
               (file-path (concat
                           org-attach-id-dir
                           "/"
                           first-part
                           "/"
                           second-part
                           "/"
                           (car files))))
          (if (= (length files) 1)
              (if (called-interactively-p)
                  (message "%s" file-path)
                file-path)
            (error "More than one attachment found!"))))
      
      (defun tuxikus/parse-ssh-config ()
        "Return a list of hosts form the tuxikus/ssh-config-file"
        (let ((ssh-config-file (expand-file-name tuxikus/ssh-config-file))
              (hosts '()))
          (with-temp-buffer
            (insert-file-contents ssh-config-file)
            (goto-char (point-min))
            (while (re-search-forward "^Host .*" nil t)
              (let ((host (nth 1 (split-string (match-string 0) " "))))
                (push host hosts))))
          hosts))
      
      (defun tuxikus/tabspaces-ssh-workspace ()
        "Create a new tabspaces workspace and connect to the selected machine via ssh in vterm"
        (interactive)
        (let ((selected-host (completing-read "Host: " (tuxikus/parse-ssh-config))))
          (tabspaces-switch-or-create-workspace (concat "ssh:" selected-host))
          ;; (unless (get-buffer selecet-host)
          ;;   (vterm selected-host))
          (vterm (concat "ssh-" selected-host))
          (vterm--goto-line -1)
          (vterm-send-string (concat "ssh " selected-host))
          (vterm-send-return)))
      
      (defun tuxikus/generate-elisp-config-header (text size)
        "Insert a header with width SIZE and the TEXT centered."
        (let* ((border-char ";")
               (border-length size)
               (border-begin-end-length 3)
               (text-length (length text))
               (padding (max 0 (/ (- border-length text-length) 2)))
               (header (concat (make-string border-begin-end-length ?\;)
                               (make-string (- border-length padding text-length border-begin-end-length) ? )
                               text
                               (make-string (- border-length padding text-length border-begin-end-length) ? )
                               (make-string border-begin-end-length ?\;))))
          (concat (make-string border-length ?\;)
                  "\n"
                  header
                  "\n"
                  (make-string border-length ?\;))))
      
      (defun tuxikus/insert-elips-config-header-large (text)
        (interactive "sText: ")
        (insert (tuxikus/generate-elisp-config-header text 100)))
      
      (defun tuxikus/insert-elips-config-header-medium (text)
        (interactive "sText: ")
        (insert (tuxikus/generate-elisp-config-header text 60)))
      
      (defun tuxikus/insert-elips-config-header-small (text)
        (interactive "sText: ")
        (insert (tuxikus/generate-elisp-config-header text 30)))
      
      (defun tuxikus/nix-flake-update ()
        (interactive)
        (async-shell-command (concat
                              "nix flake update --flake "
                              tuxikus/nix-config-directory)))
      
      (defun tuxikus/nix-rebuild-switch ()
        (interactive)
        (async-shell-command (concat
                              "sudo nixos-rebuild switch --flake "
                              tuxikus/nix-config-directory
                              ".#"
                              tuxikus/nix-flake-host)))
    '';
  };
}

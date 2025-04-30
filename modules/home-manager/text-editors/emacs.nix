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
      docker
      dockerfile-mode
      dslide
      eat
      embark
      embark-consult
      embark-org-roam
      ess
      evil
      evil-collection
      exec-path-from-shell
      exwm
      fireplace
      flycheck
      flycheck-inline
      format-all
      general
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
      ;;;                                          key config                                          ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         evil                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package evil
        :init
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        :config
        (evil-mode 1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    evil-collection                   ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package evil-collection
        :after evil
        :ensure t
        :config
        (evil-collection-init))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        general                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package general
        :after evil
        :config
        (general-create-definer tuxikus/leader-keys
          :keymaps '(normal visual emacs)
          :prefix "SPC"
          :global-prefix "C-SPC"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                       built-in packages                                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         emacs                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package emacs
        :after general
        :general
        (tuxikus/leader-keys
         "bq" 'kill-current-buffer
         "bk" 'kill-buffer
         "er" 'eval-region
         "eb" 'eval-buffer)
        :hook
        ((before-save . whitespace-cleanup)
         (makefile-mode . indent-tabs-mode)
         (prog-mode . display-line-numbers-mode)
         (git-commit-setup . tuxikus/insert-jira-ticket-number))
        :custom
        (auto-save-mode -1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        (global-auto-revert-mode 1)
        (indent-tabs-mode nil)
        (ring-bell-function 'ignore)
        (display-line-numbers-type 'relative)
        (inhibit-startup-message t)
        (inhibit-startup-screen t)
        (enable-recursive-minibuffers t)
        (read-extended-command-predicate #'command-completion-default-include-p)
        (org-id-uuid-program "~/.local/bin/uuidgenlc")
        (initial-scratch-message ";;; Emacs is fun")
        (create-lockfiles nil)
        (make-backup-files nil)
        (global-auto-revert-non-file-buffers t)
        :config
        (setq-default header-line-format mode-line-format)
        (setq-default mode-line-format nil)
        (add-to-list 'default-frame-alist
                     '(font . "Iosevka Nerd Font-${config.fontSize}")))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        compile                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package compile
        :general
        ("c" 'compile))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         files                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package files
        :general
        (tuxikus/leader-keys
          "ff" 'find-file
          "fs" 'save-buffer))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        project                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package project
        :general
        (tuxikus/leader-keys
          "pp" 'project-switch-project
          "pf" 'project-find-file))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        window                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package window
        :general
        (tuxikus/leader-keys
         "ww" 'other-window
         "w3" 'split-window-right
         "w2" 'split-window-below
         "w1" 'delete-other-windows
         "w0" 'delete-window))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       which-key                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package which-key
        :hook
        (after-init . which-key-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        ibuffer                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package ibuffer
        (tuxikus/leader-keys
          "bi" 'ibuffer))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                       external packages                                      ;;;
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
        :general
        (tuxikus/leader-keys
          "ws" 'ace-window)
        :custom
        (aw-dispatch-always t)
        (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                          avy                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package avy)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         cape                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package cape
        :bind ("M-p" . cape-prefix-map))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        consult                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package consult
        :general
        (tuxikus/leader-keys
         "sg" 'consult-grep
         "sr" 'consult-ripgrep
         "bb" 'consult-buffer
         "im" 'consult-imenu))
      
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
        :config
        (direnv-mode))
      
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
        :general
        (tuxikus/leader-keys
          "gg" 'magit))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      marginalia                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package marginalia
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
        :custom
        (org-download-method 'attach))
      
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
        :bind
        ("s-s" . tabspaces-switch-or-create-workspace)
        :hook
        (after-init . tabspaces-mode)
        :commands
        (tabspaces-switch-or-create-workspace
         tabspaces-open-or-create-project-and-workspace)
        :custom
        (tabspaces-use-filtered-buffers-as-default t)
        (tabspaces-default-tab "Default")
        (tabspaces-remove-to-default t)
        (tabspaces-include-buffers '("*scratch*" "firefox"))
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
      
      (use-package vterm)
      
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
      
      (defun tuxikus/insert-elisp-config-header-large (text)
        (interactive "sText: ")
        (insert (tuxikus/generate-elisp-config-header text 100)))
      
      (defun tuxikus/insert-elisp-config-header-medium (text)
        (interactive "sText: ")
        (insert (tuxikus/generate-elisp-config-header text 60)))
      
      (defun tuxikus/insert-elisp-config-header-small (text)
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
      
      (defun tuxikus/fix-elisp-config-header ()
        (interactive)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (looking-at "^\\s-*;;;")
              (let ((start (line-beginning-position))
                    (end (line-end-position)))
                ;; Remove leading whitespace
                (goto-char start)
                (when (re-search-forward "^\\s-+" end t)
                  (replace-match ""))))
            (forward-line 1))))
    '';

    home.file.".emacs.d/init-exwm.el".text = ''
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
        (setq exwm-input-prefix-keys
              '(?\C-x
                ?\C-u
                ?\C-h
                ?\M-x
                ?\s-s
                ?\M-`
                ?\M-&
                ?\M-:
                ?\C-\M-j  ;; Buffer list
                ?\C-\ ))  ;; Ctrl+Space
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
        (exwm-systemtray-mode 1)
        ;; Enable EXWM
        (exwm-enable))
    '';

  };
}

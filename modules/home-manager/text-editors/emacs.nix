{
  config,
  pkgs,
  lib,
  ...
}:
let
  my-emacs = config.emacsPkg;
  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (
    epkgs: with epkgs; [
      aas
      ace-window
      avy
      cape
      company
      consult
      consult-yasnippet
      corfu
      dashboard
      direnv
      docker
      dockerfile-mode
      doom-modeline
      doom-themes
      dslide
      eat
      embark
      embark-consult
      #embark-org-roam
      ess
      evil
      evil-collection
      evil-surround
      exec-path-from-shell
      fireplace
      flycheck
      format-all
      # gdscript-mode
      geiser
      geiser-guile
      general
      git-link
      go-mode
      hide-mode-line
      htmlize
      hydra
      lsp-mode
      lsp-pyright
      lua-mode
      magit
      marginalia
      meow
      move-text
      nix-mode
      orderless
      # org-download
      org-drill
      org-modern
      org-pomodoro
      # org-roam
      # org-roam-ui
      org-superstar
      # pass
      pdf-tools
      python-mode
      pyvenv
      ripgrep
      rust-mode
      salt-mode
      slime
      spacious-padding
      stimmung-themes
      surround
      tabspaces
      tuareg
      verb
      vertico
      vterm
      vundo
      walkman
      wgrep
      yasnippet
      yasnippet-snippets
      # (trivialBuild {
      #   pname = "moc";
      #   version = "v0.6.2";
      #   src = pkgs.fetchurl {
      #     url = "https://raw.githubusercontent.com/positron-solutions/moc/refs/heads/master/moc.el";
      #     sha256 = "sha256-rwsfM+FvWb0sviT2TtCVlWW8rfW6XBHlch4AbvhaL00=";
      #   };

      #   nativeBuildInputs = [ hide-mode-line ];
      # })
      (treesit-grammars.with-grammars (
        grammars: with grammars; [
          tree-sitter-bash
          tree-sitter-c
          tree-sitter-go
          tree-sitter-gomod
          tree-sitter-ocaml
          tree-sitter-python
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
      
      (require 'ucs-normalize)
      
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
      
      (defcustom tuxikus/note-system/fleeting-notes-directory nil
        "Fleeting notes directory"
        :type '(string)
        :group 'custom)
      
      (defcustom tuxikus/note-system/literature-notes-directory nil
        "Literature notes directory"
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
        (evil-set-initial-state 'git-commit-mode 'insert)
        (evil-set-initial-state 'vterm-mode 'emacs))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    evil-collection                   ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package evil-collection
        :after evil
        :config
        (evil-collection-init))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         meow                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package meow
      ;;   (require 'meow)
      ;;   (defun meow-setup ()
      ;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
      ;;     (meow-motion-define-key
      ;;      '("j" . meow-next)
      ;;      '("k" . meow-prev)
      ;;      '("<escape>" . ignore))
      ;;     (meow-leader-define-key
      ;;      ;; Use SPC (0-9) for digit arguments.
      ;;      '("1" . meow-digit-argument)
      ;;      '("2" . meow-digit-argument)
      ;;      '("3" . meow-digit-argument)
      ;;      '("4" . meow-digit-argument)
      ;;      '("5" . meow-digit-argument)
      ;;      '("6" . meow-digit-argument)
      ;;      '("7" . meow-digit-argument)
      ;;      '("8" . meow-digit-argument)
      ;;      '("9" . meow-digit-argument)
      ;;      '("0" . meow-digit-argument)
      ;;      '("/" . meow-keypad-describe-key)
      ;;      '("?" . meow-cheatsheet))
      ;;     (meow-normal-define-key
      ;;      '("0" . meow-expand-0)
      ;;      '("9" . meow-expand-9)
      ;;      '("8" . meow-expand-8)
      ;;      '("7" . meow-expand-7)
      ;;      '("6" . meow-expand-6)
      ;;      '("5" . meow-expand-5)
      ;;      '("4" . meow-expand-4)
      ;;      '("3" . meow-expand-3)
      ;;      '("2" . meow-expand-2)
      ;;      '("1" . meow-expand-1)
      ;;      '("-" . negative-argument)
      ;;      '(";" . meow-reverse)
      ;;      '("," . meow-inner-of-thing)
      ;;      '("." . meow-bounds-of-thing)
      ;;      '("[" . meow-beginning-of-thing)
      ;;      '("]" . meow-end-of-thing)
      ;;      '("a" . meow-append)
      ;;      '("A" . meow-open-below)
      ;;      '("b" . meow-back-word)
      ;;      '("B" . meow-back-symbol)
      ;;      '("c" . meow-change)
      ;;      '("d" . meow-delete)
      ;;      '("D" . meow-backward-delete)
      ;;      '("e" . meow-next-word)
      ;;      '("E" . meow-next-symbol)
      ;;      '("f" . meow-find)
      ;;      '("g" . meow-cancel-selection)
      ;;      '("G" . meow-grab)
      ;;      '("h" . meow-left)
      ;;      '("H" . meow-left-expand)
      ;;      '("i" . meow-insert)
      ;;      '("I" . meow-open-above)
      ;;      '("j" . meow-next)
      ;;      '("J" . meow-next-expand)
      ;;      '("k" . meow-prev)
      ;;      '("K" . meow-prev-expand)
      ;;      '("l" . meow-right)
      ;;      '("L" . meow-right-expand)
      ;;      '("m" . meow-join)
      ;;      '("n" . meow-search)
      ;;      '("o" . meow-block)
      ;;      '("O" . meow-to-block)
      ;;      '("p" . meow-yank)
      ;;      '("q" . meow-quit)
      ;;      '("Q" . meow-goto-line)
      ;;      '("r" . meow-replace)
      ;;      '("R" . meow-swap-grab)
      ;;      '("s" . meow-kill)
      ;;      '("t" . meow-till)
      ;;      '("u" . meow-undo)
      ;;      '("U" . meow-undo-in-selection)
      ;;      '("v" . meow-visit)
      ;;      '("w" . meow-mark-word)
      ;;      '("W" . meow-mark-symbol)
      ;;      '("x" . meow-line)
      ;;      '("X" . meow-goto-line)
      ;;      '("y" . meow-save)
      ;;      '("Y" . meow-sync-grab)
      ;;      '("z" . meow-pop-selection)
      ;;      '("'" . repeat)
      ;;      '("<escape>" . ignore)))
      ;;   (meow-setup))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        general                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
       (use-package general
        :after evil
        :config
        (general-create-definer tuxikus/leader-key
          :prefix "C-c")
       (general-create-definer tuxikus/search-leader-key
          :prefix "M-s")
        (general-create-definer tuxikus/evil-leader-key
          :keymaps '(normal visual emacs)
          :prefix "SPC")
        (general-create-definer tuxikus/evil-special-leader-key
          :keymaps '(normal visual emacs)
          :prefix "C-SPC"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                       built-in packages                                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         emacs                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package emacs
        :general
        (tuxikus/leader-key
          "er" 'eval-region
          "eb" 'eval-buffer)
        (tuxikus/evil-leader-key
          "bq" 'kill-current-buffer
          "bk" 'kill-buffer
          "er" 'eval-region
          "eb" 'eval-buffer)
        :hook
        ((before-save . whitespace-cleanup)
         (makefile-mode . indent-tabs-mode)
         ;;(prog-mode . display-line-numbers-mode)
         (git-commit-setup . tuxikus/insert-jira-ticket-number)
         (after-init . tuxikus/set-theme)
         (after-init . tuxikus/set-font))
        :custom
        (auto-save-mode nil)
        (tool-bar-mode nil)
        (menu-bar-mode nil)
        (scroll-bar-mode nil)
        (global-auto-revert-mode t)
        (indent-tabs-mode nil)
        (ring-bell-function 'ignore)
        (compilation-ask-about-save nil)
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
        (fill-column 100))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         misc                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package misc
        :general
        (tuxikus/leader-key
          "dl" 'duplicate-line))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        kmacro                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package kmacro
        :init
        (global-set-key (kbd "<f1>") 'kmacro-start-macro)
        (global-set-key (kbd "<f4>") 'kmacro-end-macro)
        (global-set-key (kbd "<f2>") 'kmacro-call-macro))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        replace                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package replace
        :general
        (tuxikus/evil-leader-key
          "trb" 'query-replace
          "trr" 'query-replace-regexp))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      elisp-mode                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package elisp-mode
        :hook
        (elisp-mode . tuxikus/set-lisp-whitespace-line-column))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       lisp-mode                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package lisp-mode
        :hook
        (lisp-mode . tuxikus/set-lisp-whitespace-line-column))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      scheme-mode                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package scheme-mode
        :hook
        (scheme-mode . tuxikus/set-lisp-whitespace-line-column))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         dired                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dired)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      use-package                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package use-package
        :custom
        (use-package-compute-statistics t))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        compile                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package compile
        :general
        (tuxikus/leader-key
          "c" 'compile)
        (tuxikus/evil-leader-key
          "cc" 'compile))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      newcomment                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package newcomment
        :general
        (tuxikus/evil-leader-key
          "tc" 'comment-dwim))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        simple                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package simple
        :general
        (tuxikus/evil-leader-key
          "SPC" 'execute-extended-command)
        (tuxikus/evil-special-leader-key
          "c" 'shell-command
          "C" 'async-shell-command))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         files                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package files
        :general
        (tuxikus/leader-key
          "ff" 'find-file
          "fs" 'save-buffer)
        (tuxikus/evil-leader-key
          "ff" 'find-file
          "fs" 'save-buffer))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       register                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package register
        :general
        (tuxikus/evil-leader-key
          "rs" 'copy-to-register
          "rb" 'bookmark-jump
          "rm" 'bookmark-set
          "ri" 'insert-register
          "rj" 'jump-to-register
          "rp" 'point-to-register
          "rl" 'list-registers))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        project                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package project
        :general
        (tuxikus/evil-leader-key
          "pp" 'project-switch-project
          "pf" 'project-find-file))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        window                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package window
        :general
        (tuxikus/evil-leader-key
          "ww" 'other-window
          "w3" 'split-window-right
          "w2" 'split-window-below
          "w1" 'delete-other-windows
          "w0" 'delete-window
          "wq" 'delete-window))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       help-fns                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package help-fns
        :general
        (tuxikus/evil-leader-key
          "hi" 'info
          "hf" 'describe-function
          "hv" 'describe-variable
          "hm" 'describe-mode
          "hk" 'describe-key))
      
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
        :general
        (tuxikus/evil-leader-key
          "bi" 'ibuffer))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       em-banner                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package em-banner)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         eglot                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package eglot
      ;;   :custom
      ;;   (eglot-autoshutdown t)
      ;;   (eglot-confirm-server-initiated-edits nil))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       savehist                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package savehist
        :hook
        (after-init . savehist-mode))
      
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
        (:keymaps 'global
                  "M-o" 'ace-window)
        (tuxikus/evil-leader-key
          "ws" 'ace-window)
        :custom
        (aw-dispatch-always t)
        (aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                          avy                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package avy
        :general
        (:keymaps 'global
                  "M-g f" 'avy-goto-line)
        (tuxikus/evil-leader-key
          "al" 'avy-goto-line
          "as" 'avy-goto-char-timer))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        geiser                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package geiser)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                     geiser-guile                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package geiser-guile)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         cape                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package cape
      ;;   :bind ("M-p" . cape-prefix-map))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        consult                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package consult
        :general
        (:keymaps 'global
                  "C-x b" 'consult-buffer
                  "M-g i" 'consult-imenu
                  "M-y" 'consult-yank-from-kill-ring)
        (tuxikus/search-leader-key
          "s" 'consult-grep
          "r" 'consult-ripgrep
          "g" 'consult-git-grep
          "i" 'consult-imenu
          "l" 'consult-line
          "c" 'consult-compile-error
          "m" 'consult-mark)
        (tuxikus/evil-leader-key
          "sg" 'consult-grep
          "sr" 'consult-ripgrep
          "bb" 'consult-buffer
          "sim" 'consult-imenu
          "sm" 'consult-mark
          "y" 'consult-yank-pop))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         slime                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package slime
        :init
        (setq inferior-lisp-program "sbcl"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                   consult-yasnippet                  ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package consult-yasnippet)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         corfu                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package corfu
      ;;   :custom
      ;;   (corfu-auto nil)
      ;;   (corfu-echo-documentation nil)
      ;;   (tab-always-indent 'complete)
      ;;   (completion-cycle-threshold nil)
      ;;   :hook
      ;;   (after-init . global-corfu-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       dashboard                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package dashboard
        :config
        (dashboard-setup-startup-hook))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       lua-mode                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package lua-mode)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       surround                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package surround
        :bind-keymap ("C-c s" . surround-keymap))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        direnv                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package direnv
        :hook
        (after-init . direnv-mode))
      
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
      
      (use-package eat)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        embark                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package embark
        :bind
        ("C-." . embark-act)
        ("M-." . embark-dwim))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                     evil-surround                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package evil-surround
        :config
        (global-evil-surround-mode 1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    embark-consult                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package embark-consult)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    embark-org-roam                   ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package embark-org-roam)
      
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
      ;;;                     doom-modeline                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package doom-modeline
        :hook
        (after-init . doom-modeline-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      doom-themes                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package doom-themes)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       fireplace                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package fireplace)
      
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
      
      ;; (use-package go-mode
      ;;   :mode "\\.go\\'")
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      go-ts-mode                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package go-ts-mode
        :mode "\\.go\\'"
        :hook
        (go-ts-mode . (lambda ()
                        (add-hook 'before-save-hook #'gofmt nil t))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    hide-mode-line                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package hide-mode-line)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        htmlize                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package htmlize)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         magit                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package magit
        :general
        (tuxikus/evil-leader-key
          "gg" 'magit
          "gs" 'magit-stage
          "gc" 'magit-commit
          "gp" 'magit-push)
        :custom
        (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                    stimmung-themes                   ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package stimmung-themes)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      marginalia                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package marginalia
        :init
        (marginalia-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                          moc                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package moc)
      
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
      ;;;                     gdscript-mode                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package gdscript-mode)
      
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
        :general
        (tuxikus/evil-leader-key
          "obt" 'org-babel-tangle
          "ol" 'org-insert-link
          "oh" 'org-insert-heading
          "os" 'org-insert-subheading
          "ot" 'org-todo
          "oo" 'org-open-at-point
          ;; "orf" 'org-roam-node-find
          ;; "ori" 'org-roam-node-insert
          "oci" 'org-clock-in
          "oco" 'org-clock-out
          "oe" 'org-edit-special)
        :hook
        ((org-mode . auto-fill-mode)
         (org-mode . (lambda ()
                       (add-hook 'after-save-hook 'tuxikus/auto-tangle nil t))))
        :custom
        ((org-attach-id-dir "~/org/.attach")
         (org-log-done 'time)
         (org-confirm-babel-evaluate nil)
         (org-hide-emphasis-markers t)
         (org-imenu-depth 7)
         (org-latex-image-default-scale 2)
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
                                     (lisp . t)
                                     (python . t)
                                     (lua . t)
                                     (R . t)
                                     (scheme . t)
                                     (dot . t)
                                     (gnuplot . t))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                     org-download                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package org-download
      ;;   :custom
      ;;   (org-download-method 'attach))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      org-modern                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-modern
        :if (display-graphic-p)
        :hook
        (org-mode . org-modern-mode)
        (org-agenda-finalize . org-modern-agenda))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       org-roam                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package org-roam
      ;;   :bind
      ;;   (("C-c r f" . org-roam-node-find)
      ;;    ("C-c r i" . org-roam-node-insert))
      ;;   :init
      ;;   (setq tuxikus/note-system/fleeting-notes-directory (concat org-roam-directory "/fleeting-notes"))
      ;;   (setq tuxikus/note-system/literature-notes-directory (concat org-roam-directory "/literature-notes"))
      ;;   :custom
      ;;   (org-roam-directory (concat org-directory "/roam"))
      ;;   (org-roam-capture-templates
      ;;    '(("d" "default"
      ;;       plain "%?"
      ;;       :target (file+head "%<%Y-%m-%d>-''${slug}.org"
      ;;                          "#+title: ''${title}\n#+author: tuxikus\n#+date: <%<%Y-%m-%d %a>>\n#+startup: latexpreview\n#+filetags:\n\n\n* Siehe auch\n* Referenzen\n")
      ;;       :unnarrowed t)))
      ;;   (org-roam-dailies-capture-templates
      ;;    '(("d" "default" entry "* %<%H:%M %p>: %?"
      ;;       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
      ;;   (org-roam-node-display-template (concat "''${title:*} " (propertize "''${tags:10}" 'face 'org-tag)))
      ;;   (org-roam-db-autosync-mode t)
      ;;   (org-roam-completion-everywhere t))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      org-roam-ui                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; (use-package org-roam-ui)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                     org-superstar                    ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-superstar
        :hook
        (org-mode . org-superstar-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                     org-pomodoro                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package org-pomodoro
        :custom
        ((org-pomodoro-length 25)
         (org-pomodoro-short-break-length 5)
         (org-pomodoro-long-break-length 20)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        tuareg                        ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package tuareg
        :mode (("\\.ml\\'" . tuareg-mode)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                         pass                         ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;;(use-package pass)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       pdf-tools                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package pdf-tools
        :hook
        (after-init . pdf-tools-install))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                        company                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package company
        :hook
        (after-init . global-company-mode))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       lsp-mode                       ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package lsp-mode
        :general
        (tuxikus/leader-key
          "lr" 'lsp-rename
          "ld" 'lsp-find-definition
          "lu" 'lsp-find-references
          "la" 'lsp-execute-code-action)
        :hook ((python-mode
                python-ts-mode
                go-mode
                go-ts-mode) . lsp-deferred))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      lsp-pyright                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
       (use-package lsp-pyright
        :ensure t
        :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
        :hook (python-mode . (lambda ()
                                (require 'lsp-pyright)
                                (lsp))))  ; or lsp-deferred
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                      python-mode                     ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package python-mode
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
        :mode "\\.rs\\'")
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                       salt-mode                      ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package salt-mode
        :mode "\\.sls\\'")
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                   spacious-padding                   ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (use-package spacious-padding
        :custom
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
        (tabspaces-todo-file-name "project-todo.org")
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
        :hook
        (after-init . vertico-mode))
      
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
        :custom
        (yas-global-mode t))
      
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
      
      (defun tuxikus/set-font ()
        (add-to-list 'default-frame-alist
                     '(font . "Iosevka Nerd Font-${config.fontSize}")))
      
      (defun tuxikus/set-theme ()
        (load-theme 'modus-operandi))
      
      (defun tuxikus/get-current-branch ()
        (interactive)
        (if (called-interactively-p)
            (message (magit-get-current-branch)
        (magit-get-current-branch))))
      
      (defun tuxikus/get-jira-ticket-number (branch)
        (interactive)
        (when (string-match "MW[A-Z]+-[0-9]+" branch)
          (match-string 0 branch)))
      
      (defun tuxikus/insert-jira-ticket-number ()
        (interactive)
        (let* ((branch (magit-get-current-branch))
              (jira-ticket-number (tuxikus/get-jira-ticket-number branch)))
          (message jira-ticket-number)
          (when jira-ticket-number
            (insert (concat (tuxikus/get-jira-ticket-number (magit-get-current-branch)) ": ")))))
      
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
                org-roam-db-location (concat org-roam-directory "/org-roam.db"))))
      
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
              (if (called-interactively-p)y
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
                (goto-char start)
                (when (re-search-forward "^\\s-+" end t)
                  (replace-match ""))))
            (forward-line 1))))
      
      (defun tuxikus/set-default-whitespace-line-colum ()
        (setq whitespace-line-column 80))
      
      (defun tuxikus/set-lisp-whitespace-line-column ()
        (setq whitespace-line-column 100))
      
      (defun tuxikus/get-org-title (file)
      (with-temp-buffer
        (condition-case nil
            (progn
              (insert-file-contents file)
              (goto-char (point-min))
              (if (re-search-forward "^#\\+[Tt][Ii][Tt][Ll][Ee]:?[ \t]*\\(.*\\)$" nil t)
                  (match-string 1)
                ""))
          (error ""))))
      
      (defun tuxikus/note-system/insert-literature-note-link ()
        (interactive)
        (let* ((note-path (read-file-name "Select Literature note: "))
               (note-title (tuxikus/get-org-title note-path)))
          (org-insert-link nil
                           (concat "file:" note-path)
                           (concat "Literature note: " note-title))))
      
      (defun tuxikus/note-system/insert-fleeting-note-link ()
        (interactive)
        (let* ((note-path (read-file-name "Select Fleeting note: "))
               (note-title (tuxikus/get-org-title note-path)))
          (org-insert-link nil
                           (concat "file:" note-path)
                           (concat "Fleeting note: " note-title))))
      
      (defun tuxikus/slugify (s)
        (let* ((s (downcase s))
               (s (ucs-normalize-NFD-string s))
               (s (replace-regexp-in-string "[\u0300-\u036f]" "" s))
               (s (replace-regexp-in-string "[^a-z0-9 -]" "" s))
               (s (replace-regexp-in-string "[ ]+" "-" s))
               (s (replace-regexp-in-string "-+" "-" s))
               (s (replace-regexp-in-string "^-\\|-$" "" s)))
          s))
      
      (defun tuxikus/note-system/new-note (type)
        (unless (memq type '(fleeting literature))
          (error "Invalid note type: %s" type))
        (let ((path (if (eq type 'fleeting)
                        tuxikus/note-system/fleeting-notes-directory
                      tuxikus/note-system/literature-notes-directory)))
          (let* ((note-title (read-string "Title: "))
                 (note-file-path (concat path
                                         "/"
                                         (format-time-string "%Y-%m-%d")
                                         "-"
                                         (tuxikus/slugify note-title)
                                         ".org")))
          (write-region (concat "#+title: " note-title) nil note-file-path nil)
          (find-file note-file-path))))
      
      (defun tuxikus/note-system/new-fleeting-note ()
        (interactive)
        (tuxikus/note-system/new-note 'fleeting))
      
      (defun tuxikus/note-system/new-literature-note ()
        (interactive)
        (tuxikus/note-system/new-note 'literature))
      
      (defun tuxikus/reset-emacs ()
        "This function save and kill all open buffers and open the dashboard. A simple reset."
        (interactive)
        (save-some-buffers)
        (tuxikus/kill-all-buffers)
        (dashboard-open))
      
      (defun tuxikus/kill-all-buffers ()
        ""
        (interactive)
        (mapc 'kill-buffer (buffer-list)))
      
      (defun tuxikus/auto-tangle ()
        "Tangle the Org file if it contains '#+tangle: yes' in the first line."
        (when (and (eq major-mode 'org-mode)
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^#\\+tangle: yes" nil t)))
          (org-babel-tangle)
          (message "File tangled!")))
    '';
  };
}

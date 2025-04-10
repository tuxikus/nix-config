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
      pdf-tools
      aas
      go-mode
      olivetti
      ace-window
      avy
      cape
      consult
      consult-yasnippet
      corfu
      corfu-terminal
      dashboard
      dired-rsync
      dired-rsync-transient
      direnv
      dirvish
      docker
      dslide
      htmlize
      doom-modeline
      doom-themes
      eat
      embark
      embark-consult
      embark-org-roam
      ess
      rust-mode
      dockerfile-mode
      fireplace
      flycheck
      flycheck-inline
      format-all
      git-link
      keycast
      magit
      marginalia
      move-text
      nix-mode
      orderless
      org-modern
      org-present
      org-roam
      org-superstar
      perspective
      python-mode
      pyvenv
      ripgrep
      salt-mode
      spacious-padding
      verb
      vertico
      vundo
      walkman
      wgrep
      org-download
      org-roam-ui
      yasnippet
      hide-mode-line
      (trivialBuild {
        pname = "moc";
        version = "v0.6.2";
        src = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/positron-solutions/moc/refs/heads/master/moc.el";
          sha256 = "sha256-rwsfM+FvWb0sviT2TtCVlWW8rfW6XBHlch4AbvhaL00=";
        };

        nativeBuildInputs = [ hide-mode-line ];
      })

      (trivialBuild {
        pname = "zellij";
        version = "master";

        src = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/tuxikus/zellijel/refs/heads/main/zellij.el";
          sha256 = "sha256-eT2qoXUl4Lc8WgmtGp1PxICZHmhyNVqIWeqjGRB48Kc=";
        };
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
    (use-package emacs
      :bind
      (("M-<tab>" . completion-at-point)
       ("C-c d" . duplicate-line)
       ("C-c f f" . find-file)
       ("C-c f a" . format-all-buffer)
       ("C-c e r" . eval-region)
       ("C-c e b" . eval-buffer)
       ("C-c w m" . whitespace-mode)
       ("C-x c" . compile)
       ("C-S-a" . beginning-of-buffer)
       ("C-S-e" . end-of-buffer))
    
      :hook
      ((before-save . whitespace-cleanup)
       (makefile-mode . indent-tabs-mode)
       (prog-mode . display-line-numbers-mode)
       (kill-emacs . persp-state-save)
       (after-save . persp-state-save))
    
      :init
      (fset 'yes-or-no-p 'y-or-n-p)
      (auto-save-mode -1)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (save-place-mode 1)
      (global-auto-revert-mode 1)
      (setq-default indent-tabs-mode nil)
    
      ;; cursor
      ;;(setq-default cursor-type 'box)
    
      ;; line numbers
      (setq display-line-numbers 'relative)
      ;;(global-display-line-numbers-mode)
    
      (load-theme 'doom-bluloco-light t)
    
      :config
      (setq create-lockfiles nil
            make-backup-files nil
            custom-theme-directory "~/.emacs.d/themes"
            inhibit-startup-message t
            inhibit-startup-screen t
            initial-scratch-message ";;; Emacs is fun"
            global-auto-revert-non-file-buffers t
            org-id-uuid-program "~/.local/bin/uuidgenlc"
            persp-state-default-file "~/.local/state/emacs/persp-state")
    
      ;; (set-frame-font "Iosevka Nerd Font-15" nil t)
    
      (add-to-list 'default-frame-alist
                   '(font . "Iosevka Nerd Font-${config.fontSize}"))
    
      (which-key-mode 1)
    
      :custom
      (enable-recursive-minibuffers t)
      (read-extended-command-predicate #'command-completion-default-include-p)
    
      ;; Emacs 30 and newer: Disable Ispell completion function.
      ;; Try `cape-dict' as an alternative.
      (text-mode-ispell-word-completion nil)
    
      ;; Hide commands in M-x which do not apply to the current mode.  Corfu
      ;; commands are hidden, since they are not used via M-x. This setting is
      ;; useful beyond Corfu.
      (read-extended-command-predicate #'command-completion-default-include-p))
    
    (use-package dirvish
      :init
      (dirvish-override-dired-mode))
    
    (use-package aas
      :hook
      ((LaTeX-mode . aas-activate-for-major-mode)
       (org-mode . aas-activate-for-major-mode))
    
      :config
      (aas-set-snippets 'text-mode
        ":o" "ö"
        ":O" "Ö"
        ":u" "ü"
        ":U" "Ü"
        ":a" "ä"
        ":A" "Ä"
        "sz" "ß"))
    
    (use-package ace-window
      :bind
      (("M-o" . ace-window))
    
      :init
      (setq aw-dispatch-always t)
      (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?f)))
    
    (use-package avy
      :bind
      (("M-g f" . avy-goto-line)
       ("M-g w" . avy-goto-word-1)
       ("C-'" . avy-goto-char-2)))
    
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
    
    (use-package consult
      :after perspective
      :bind
      (("C-c M-x" . consult-mode-command)
       ("C-c h" . consult-history)
       ("C-c k" . consult-kmacro)
       ("C-c m" . consult-man)
       ("C-c i" . consult-info)
       ("C-x b" . consult-buffer)
       ("C-x 4 b" . consult-buffer-other-window)
       ("C-x 5 b" . consult-buffer-other-frame)
       ("C-x t b" . consult-buffer-other-tab)
       ("C-x r b" . consult-bookmark)
       ("C-x p b" . consult-project-buffer)
       ("M-#" . consult-register-load)
       ("M-'" . consult-register-store)
       ("C-M-#" . consult-register)
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
       ("M-s c" . consult-locate)
       ("M-s g" . consult-grep)
       ("M-s G" . consult-git-grep)
       ("M-s r" . consult-ripgrep)
       ("M-s l" . consult-line)
       ("M-s L" . consult-line-multi)
       ("M-s k" . consult-keep-lines)
       ("M-s u" . consult-focus-lines))
    
      :config
      (consult-customize consult--source-buffer :hidden t :default nil)
      (add-to-list 'consult-buffer-sources persp-consult-source))
    
    (use-package corfu
      :init
      (unless (display-graphic-p)
        (corfu-terminal-mode +1))
      :config
      (global-corfu-mode)
      :custom
      (corfu-auto nil)
      (corfu-echo-documentation nil)
      (tab-always-indent 'complete)
      (completion-cycle-threshold nil))
    
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
    
    ;; (use-package dired
    ;;   :config
    ;;   (put 'dired-find-alternate-file 'disabled nil))
    
    (use-package direnv
      :init
      (direnv-mode))
    
    (use-package pdf-tools
      :init
      (pdf-tools-install))
    
    (defcustom container-executable 'podman
      "The executable to be used with docker mode."
      :type '(choice
              (const :tag "docker" docker)
              (const :tag "podman" podman))
      :group 'custom)
    
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
               docker-container-tramp-methodu "podman"))))
    
    (use-package doom-modeline
      :init
      (setq doom-modeline-time t
            doom-modeline-env-version t)
    
      (doom-modeline-mode 1))
    
    (use-package doom-themes)
    
    (use-package eglot
      ;; :hook
      ;; ((python-ts-mode . eglot-ensure)
      ;;  (python-mode . eglot-ensure))
      :custom
      (eglot-autoshutdown t)
      (eglot-confirm-server-initiated-edits nil))
    
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
    
    (use-package embark
      :bind
      ("C-." . embark-act)
      ("M-." . embark-dwim))
    
    (use-package em-banner)
    
    (use-package flycheck
      :hook
      (after-init . global-flycheck-mode))
    
    (use-package flycheck-inline
      :config
      (with-eval-after-load 'flycheck
        (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))
    
    (use-package format-all)
    
    (use-package magit)
    
    (use-package marginalia
      :bind (:map minibuffer-local-map
                  ("M-A" . marginalia-cycle))
      :init
      (marginalia-mode))
    
    (use-package move-text
      :init
      (move-text-default-bindings))
    
    (use-package nix-mode
      :mode "\\.nix\\'")
    
    (use-package orderless
      :custom
      (completion-styles '(orderless flex))
      (completion-category-defaults nil)
      (completion-category-overrides '((file (styles basic partial-completion)))))
    
    (use-package org
      :bind
      ("C-M-<return>" . org-insert-subheading)
      :init
      (setq org-attach-id-dir "~/org/.attach"
            org-log-done 'time
            org-hide-emphasis-markers t
            org-imenu-depth 7)
    
      (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                               (emacs-lisp . t)
                                                               (python . t))))
    
    
    (use-package org-roam
      :custom
      (org-roam-directory (concat org-directory "/roam"))
      :config
      ;; If you're using a vertical completion framework, you might want a more informative completion interface
      ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
      (org-roam-db-autosync-mode)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))
    
    (use-package org-modern
      :ensure t
      :defer t
      :hook
      (org-mode . org-modern-mode)
      (org-agenda-finalize . org-modern-agenda))
    
    (use-package org-superstar
      :hook
      (org-mode . (lambda () (org-superstar-mode 1))))
    
    (use-package org-present)
    
    (use-package perspective
      :custom
      (persp-mode-prefix-key (kbd "C-c M-p"))
      :init
      (persp-mode))
    
    (use-package salt-mode
      :hook
      (salt-mode . (lambda () (flyspell-mode 1))))
    
    (use-package spacious-padding
      :init
      (setq spacious-padding-widths
            '( :internal-border-width 15
               :header-line-width 4
               :mode-line-width 6
               :tab-width 4
               :right-divider-width 30
               :scroll-bar-width 8
               :fringe-width 8))
    
      ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
      ;; is very flexible and provides several examples.
      (setq spacious-padding-subtle-mode-line
            `( :mode-line-active 'default
               :mode-line-inactive vertical-border))
    
      (spacious-padding-mode 1))
    
    (use-package savehist
      :init
      (savehist-mode))
    
    (use-package org-download)
    
    (use-package go-mode)
    
    (use-package treesit
      :init
      (setq major-mode-remap-alist
            '((bash-mode . bash-ts-mode)
              (python-mode . python-ts-mode)
              (go-mode . go-ts-mode)
              (rust-mode . rust-ts-mode))))
    
    (use-package vertico
      :custom
      (vertico-scroll-margin 0) ;; Different scroll margin
      (vertico-count 20) ;; Show more candidates
      ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
      (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
      :init
      (vertico-mode))
    
    (use-package yasnippet
      :init
      (yas-global-mode 1))
    
    (use-package zellij)
    
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
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
    '';
  };
}

{ pkgs, ... }:
let
  my-emacs = pkgs.emacs.override {
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
    dashboard
    direnv
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
    libmpdel
    magit
    marginalia
    mpdel
    nix-mode
    orderless
    org-roam
    org-superstar
    perspective
    python-mode
    pyvenv
    ripgrep
    salt-mode
    verb
    vertico
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
  programs.emacs = {
    enable = true;
    package = my-emacs-with-packages;
    extraConfig = ''
      (load-file "~/.emacs.d/init.el")
    '';
  };

  home.file.".emacs.d/init.el" = {
    source = ./init.el;
  };

  home.file.".emacs.d/lisp" = {
    source = ./lisp;
  };

  home.file.".emacs.d/snippets" = {
    source = ./snippets;
  };
  
  home.file.".emacs.d/themes" = {
    source = ./themes;
  };
}

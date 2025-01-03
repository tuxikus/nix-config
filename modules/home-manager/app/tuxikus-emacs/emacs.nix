{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.vertico
      epkgs.use-package
      epkgs.orderless
      epkgs.company
      epkgs.consult
      epkgs.magit
      epkgs.ace-window
      epkgs.avy
      epkgs.wgrep
      epkgs.org-roam
      epkgs.fireplace
      epkgs.projectile
      epkgs.spacious-padding
      epkgs.marginalia
      epkgs.yasnippet
      epkgs.ripgrep
      epkgs.haskell-mode
      epkgs.nix-mode
      epkgs.salt-mode
      epkgs.python-mode
      epkgs.eat
      epkgs.dashboard
      epkgs.restart-emacs
      epkgs.doom-modeline
      epkgs.mpdel
      epkgs.libmpdel
      epkgs.embark
      epkgs.embark-org-roam
      epkgs.embark-consult
    ];
    
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

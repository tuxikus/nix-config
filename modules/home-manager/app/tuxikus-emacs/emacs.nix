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
      epkgs.org-roam
      epkgs.fireplace
      epkgs.projectile
      epkgs.spacious-padding

      epkgs.yasnippet

      epkgs.haskell-mode
      epkgs.nix-mode
      epkgs.salt-mode
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
}

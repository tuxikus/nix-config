{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      ### ui
      epkgs.spacious-padding
      epkgs.doom-modeline
      epkgs.dashboard
      epkgs.org-superstar
      
      ### completion
      epkgs.vertico
      epkgs.orderless
      epkgs.marginalia
      epkgs.corfu
      epkgs.cape
      epkgs.consult
      epkgs.flycheck
      
      ### modes
      epkgs.haskell-mode
      epkgs.nix-mode
      epkgs.salt-mode
      epkgs.python-mode
      
      ### project management
      epkgs.projectile
      
      ### window management
      epkgs.ace-window
      
      ### snippets
      epkgs.yasnippet
      
      ### org
      epkgs.org-roam

      ### util
      epkgs.dwim-shell-command
      epkgs.magit
      epkgs.avy
      epkgs.wgrep
      epkgs.ripgrep
      epkgs.eat
      epkgs.direnv
      epkgs.embark
      epkgs.embark-org-roam
      epkgs.embark-consult
      epkgs.treesit-auto
      
      ### media
      epkgs.mpdel
      epkgs.libmpdel      

      ### fun
      epkgs.fireplace
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

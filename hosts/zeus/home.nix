{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  homeManagerModulesDirectory = ../../modules/home-manager;
in
{
  imports = [
    # modules
    (homeManagerModulesDirectory + "/shell/bash.nix")
    (homeManagerModulesDirectory + "/shell/xonsh.nix")

    (homeManagerModulesDirectory + "/app/tuxikus-emacs/emacs.nix")
    (homeManagerModulesDirectory + "/app/tuxikus-emacs/emacs-daemon.nix")
    (homeManagerModulesDirectory + "/app/nyxt/nyxt.nix")
    (homeManagerModulesDirectory + "/app/nixvim/nixvim.nix")
    (homeManagerModulesDirectory + "/app/ghostty.nix")
    (homeManagerModulesDirectory + "/app/fuzzel.nix")
    (homeManagerModulesDirectory + "/latex.nix")
    (homeManagerModulesDirectory + "/wm/hyprland.nix")
    
    # scripts
    (homeManagerModulesDirectory + "/bin/home-backup.nix")
  ];

  home.username = "tuxikus";
  home.homeDirectory = "/home/tuxikus";
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.05";

  ### module config
  # /wm/hyprpaper.nix
  wallpaper = ./assets/wallpaper.png;
  terminal = "ghostty";
  appLauncher = "fuzzel";

  # Packages
  home.packages = [ ];

  # Programs
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userEmail = "contact@tuxikus.de";
    userName = "tuxikus";
  };

  # add ~/.local/bin to PATH
  home.sessionPath = [ "$HOME/.local/bin" ];
}

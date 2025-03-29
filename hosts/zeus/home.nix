{
  pkgs,
  ...
}:
let
  homeManagerModulesDirectory = ../../modules/home-manager;
in
{
  imports = [
    (homeManagerModulesDirectory + "/shell/bash.nix")
    (homeManagerModulesDirectory + "/shell/xonsh.nix")
    (homeManagerModulesDirectory + "/text-editors/emacs.nix")
    (homeManagerModulesDirectory + "/gui-apps/nyxt.nix")
    (homeManagerModulesDirectory + "/gui-apps/qutebrowser.nix")
    (homeManagerModulesDirectory + "/gui-apps/ghostty.nix")
    (homeManagerModulesDirectory + "/gui-apps/fuzzel.nix")
    (homeManagerModulesDirectory + "/utility/tmux.nix")
    (homeManagerModulesDirectory + "/utility/zellij.nix")
    (homeManagerModulesDirectory + "/education/latex.nix")
    (homeManagerModulesDirectory + "/window-manager/hyprland.nix")
    (homeManagerModulesDirectory + "/window-manager/hyprpaper.nix")
    (homeManagerModulesDirectory + "/window-manager/waybar.nix")
    (homeManagerModulesDirectory + "/window-manager/qtile.nix")
    (homeManagerModulesDirectory + "/scripts/home-backup.nix")
    (homeManagerModulesDirectory + "/scripts/uuidgenlc.nix")

  ];

  home = {
    username = "tuxikus";
    homeDirectory = "/home/tuxikus";
    
    sessionVariables = {
      EDITOR = "emacsclient -c";
      VISUAL = "emacsclient -c";
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "24.05";

    packages = [];

    sessionPath = [ "$HOME/.local/bin" ];
  };

  services = {
    emacs.enable = true;
  };

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userEmail = "contact@tuxikus.de";
      userName = "tuxikus";
    };
  };

  wallpaper = ./assets/wallpaper.png;
  terminal = "ghostty";
  appLauncher = "fuzzel";

  emacsPkg = pkgs.emacs;
  customInit = ''
      (setq container-executable 'podman)
    '';
  fontSize = "15";

  qtileWallpaper = ./assets/qtile-wallpaper.png;
}

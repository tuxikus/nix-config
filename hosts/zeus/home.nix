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
    (homeManagerModulesDirectory + "/shell/fish.nix")
    (homeManagerModulesDirectory + "/text-editors/emacs.nix")
    (homeManagerModulesDirectory + "/gui-apps/ghostty.nix")
    (homeManagerModulesDirectory + "/gui-apps/wezterm.nix")
    (homeManagerModulesDirectory + "/gui-apps/fuzzel.nix")
    (homeManagerModulesDirectory + "/utility/tmux.nix")
    (homeManagerModulesDirectory + "/utility/zellij.nix")
    (homeManagerModulesDirectory + "/education/latex.nix")
    (homeManagerModulesDirectory + "/window-manager/hyprland.nix")
    (homeManagerModulesDirectory + "/window-manager/hyprpaper.nix")
    (homeManagerModulesDirectory + "/window-manager/hyprlock.nix")
    (homeManagerModulesDirectory + "/window-manager/waybar.nix")
    (homeManagerModulesDirectory + "/scripts/home-backup.nix")
    (homeManagerModulesDirectory + "/scripts/borg-home-backup.nix")
    (homeManagerModulesDirectory + "/scripts/uuidgenlc.nix")
    (homeManagerModulesDirectory + "/scripts/music-unzip.nix")
    (homeManagerModulesDirectory + "/desktop-entries/shutdown.nix")
    (homeManagerModulesDirectory + "/desktop-entries/reboot.nix")
    (homeManagerModulesDirectory + "/desktop-entries/clear-cliphist.nix")
  ];

  home = {
    username = "tuxikus";
    homeDirectory = "/home/tuxikus";

    sessionVariables = {
      EDITOR = "emacsclient";
      VISUAL = "emacsclient";
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

  # services = {
  #   emacs.enable = true;
  # };

  programs = {
    home-manager.enable = true;
    password-store = {
      enable = true;
      package = pkgs.pass-wayland.withExtensions (exts:
        with exts; [
          pass-otp
        ]);
      settings = {
        PASSWORD_STORE_DIR = "$HOME/.password-store";
      };
    };
    git = {
      enable = true;
      userEmail = "contact@tuxikus.de";
      userName = "tuxikus";
    };
  };

  wallpaper = ./assets/wallpaper.jpg;
  terminal = "ghostty";
  appLauncher = "fuzzel";

  emacsPkg = (pkgs.emacs.override { withNativeCompilation = true; });
  customInit = ''
      (setq container-executable 'podman)
    '';
  fontSize = "15";
}

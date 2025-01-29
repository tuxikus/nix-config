{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;

  fonts.packages = [
    pkgs.nerdfonts
  ];

  environment.systemPackages = [
    pkgs.raycast
    pkgs.btop
    pkgs.alacritty
    pkgs.aerospace
    pkgs._1password-cli
    pkgs.sketchybar
    pkgs.jankyborders
    pkgs.gcc
    pkgs.fzf
    pkgs.go-task
    pkgs.python3
    pkgs.openssh
    pkgs.jupyter
    pkgs.tree-sitter

    pkgs.fishPlugins.fzf-fish
    pkgs.fishPlugins.puffer
  ];

  environment.shells = [
    pkgs.bashInteractive
  ];

  homebrew = {
    enable = true;
    onActivation.cleanup = "uninstall";
    taps = [];
    brews = [];
    casks = [
      "orbstack"
      "tunnelblick"
      "utm"
      "firefox"
    ];
  };

  programs.fish.enable = true;
  programs.bash.enable = true;

  services.nix-daemon.enable = true;
  services.emacs.enable = true;
  services.aerospace.enable = true;
  services.sketchybar.enable = true;
  services.jankyborders.enable = true;
  #services.karabiner-elements.enable = true;

  nix.settings.experimental-features = "nix-command flakes";

  programs.zsh.enable = true;

  # Used for backwards compatibility, please read the changelog before changing
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  nixpkgs.hostPlatform = "aarch64-darwin";

  users.users."dominik.potoczki" = {
    name = "dominik.potoczki";
    home = "/Users/dominik.potoczki";
  };

  system.defaults.screencapture.target = "clipboard";

  security.pam.enableSudoTouchIdAuth = true;
}

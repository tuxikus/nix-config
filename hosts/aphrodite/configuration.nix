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
    pkgs.fzf
    pkgs.aerospace
    pkgs._1password-cli
    pkgs.sketchybar
    pkgs.jankyborders
    pkgs.gcc
  ];

  services.nix-daemon.enable = true;
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

  security.pam.enableSudoTouchIdAuth = true;
}

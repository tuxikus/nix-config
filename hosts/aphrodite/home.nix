{ config, pkgs, ... }:
let
  homeManagerModulesDirectory = ../../modules/home-manager;
in
{
  imports = [
    (homeManagerModulesDirectory + "/app/tuxikus-emacs/emacs.nix")
    (homeManagerModulesDirectory + "/app/nixvim/nixvim.nix")
    (homeManagerModulesDirectory + "/shell/fish.nix")
  ];
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.

  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  home.packages = [];

  programs.home-manager.enable = true;
}

  

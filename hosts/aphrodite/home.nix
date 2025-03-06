{ pkgs, ... }:
let
  homeManagerModulesDirectory = ../../modules/home-manager;
in
{
  imports = [
    (homeManagerModulesDirectory + "/gui-apps/emacs.nix")
    (homeManagerModulesDirectory + "/gui-apps/qutebrowser.nix")
    (homeManagerModulesDirectory + "/shell/xonsh.nix")
    (homeManagerModulesDirectory + "/shell/bash.nix")
  ];
  home = {
    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
  
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "24.11"; # Please read the comment before changing.
    packages = [];
  };
  programs.home-manager.enable = true;
  emacsPkg = pkgs.emacs-macport;
  fontSize = "200";
  customInit = ''
  (setq custom-init-loaded t)
  (setq mac-option-key-is-meta t
  	mac-command-key-is-meta nil
  	mac-option-modifier 'meta
  	mac-command-modifier 'super)
  (setq container-executable 'docker)
  '';
}

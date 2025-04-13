{ pkgs, ... }:
let
  homeManagerModulesDirectory = ../../modules/home-manager;
in
{
  imports = [
    (homeManagerModulesDirectory + "/text-editors/emacs.nix")
    (homeManagerModulesDirectory + "/education/latex.nix")
    (homeManagerModulesDirectory + "/education/r.nix")
    (homeManagerModulesDirectory + "/shell/xonsh.nix")
    (homeManagerModulesDirectory + "/shell/bash.nix")
    (homeManagerModulesDirectory + "/utility/tmux.nix")
    (homeManagerModulesDirectory + "/gui-apps/ghostty.nix")
    (homeManagerModulesDirectory + "/scripts/uuidgenlc.nix")
  ];

  home = {
    sessionVariables = {
      EDITOR = "emacsclient -c";
      VISUAL = "emacsclient -c";
    };
    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.

    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "24.11"; # Please read the comment before changing.
    packages = [ ];
  };

  programs.home-manager.enable = true;

  #emacsPkg = pkgs.emacs-macport;
  emacsPkg = (emacs.override { withNativeCompilation = false; });
  fontSize = "20";
  customInit = ''
    (setq custom-init-loaded t)
    (setq mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-option-modifier 'meta
      mac-command-modifier 'super)
    (setq container-executable 'docker)
  '';
}

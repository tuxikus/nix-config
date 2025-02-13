{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;

  fonts.packages = [
    pkgs.nerdfonts
  ];

  environment.systemPackages = with pkgs; [
    raycast
    btop
    alacritty
    aerospace
    _1password-cli
    sketchybar
    jankyborders
    gcc
    fzf
    go-task
    python3
    openssh
    jupyter
    tree-sitter
    poppler_utils
    dig
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

  programs.bash.enable = true;

  services.nix-daemon.enable = true;

  services.aerospace = {
    enable = true;
    settings = {
      gaps = {
        inner.horizontal = 22;
        inner.vertical = 22;
        outer.left = 15;
        outer.bottom = 15;
        outer.top = [ { monitor."T34w-30" = 50; } 15 ];
        outer.right = 15;
      };
      mode.main.binding = {
        cmd-left = "focus left";
        cmd-down = "focus down";
        cmd-up = "focus up";
        cmd-right = "focus right";

        cmd-shift-left = "move left";
        cmd-shift-down = "move down";
        cmd-shift-up = "move up";
        cmd-shift-right = "move right";

        cmd-m = "fullscreen";

        cmd-1 = "workspace 1";
        cmd-2 = "workspace 2";
        cmd-3 = "workspace 3";
        cmd-4 = "workspace 4";
        cmd-5 = "workspace 5";
        cmd-6 = "workspace 6";
        cmd-7 = "workspace 7";
        cmd-8 = "workspace 8";
        cmd-9 = "workspace 9";
        cmd-0 = "workspace 10";
        
        cmd-shift-1 = "move-node-to-workspace 1";
        cmd-shift-2 = "move-node-to-workspace 2";
        cmd-shift-3 = "move-node-to-workspace 3";
        cmd-shift-4 = "move-node-to-workspace 4";
        cmd-shift-5 = "move-node-to-workspace 5";
        cmd-shift-6 = "move-node-to-workspace 6";
        cmd-shift-7 = "move-node-to-workspace 7";
        cmd-shift-8 = "move-node-to-workspace 8";
        cmd-shift-9 = "move-node-to-workspace 9";
        cmd-shift-0 = "move-node-to-workspace 10";

        cmd-r = "mode resize";
      };
      mode.resize.binding = {
        h = "resize width -50";
        j = "resize height +50";
        k = "resize height -50";
        l = "resize width +50";
        enter = "mode main";
        esc = "mode main";
      };
    };
  };
  services.sketchybar.enable = true;
  services.jankyborders = {
    enable = true;
    active_color = "0xFFFF0000";
    width = 7.0;
  };
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

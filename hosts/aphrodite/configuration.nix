{ pkgs, ... }:
let
  nixModulesDirectory = ../../modules/nixos;
in
{
  imports = [

  ];

  nix.settings.experimental-features = "nix-command flakes";

  nixpkgs = {
    config.allowUnfree = true;
    hostPlatform = "aarch64-darwin";
  };

  users = {
    users."dominik.potoczki" = {
      name = "dominik.potoczki";
      home = "/Users/dominik.potoczki";
    };
  };

  fonts.packages = [
    pkgs.nerdfonts
    pkgs.profont
  ];

  services = {
    nix-daemon.enable = true;

    aerospace = {
      enable = true;
      settings = {
        gaps = {
          inner.horizontal = 22;
          inner.vertical = 22;
          outer.left = 15;
          outer.bottom = 15;
          outer.top = [
            { monitor."T34w-30" = 50; }
            15
          ];
          outer.right = 15;
        };
        mode.main.binding = {
          cmd-h = "focus left";
          cmd-j = "focus down";
          cmd-k = "focus up";
          cmd-l = "focus right";

          cmd-shift-h = "move left";
          cmd-shift-j = "move down";
          cmd-shift-k = "move up";
          cmd-shift-l = "move right";

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
        };
      };
    };
    sketchybar.enable = true;

    jankyborders = {
      enable = true;
      active_color = "0xFFFF0000";
      width = 10.0;
    };
  };

  environment = {
    systemPackages = with pkgs; [
      _1password-cli
      aerospace
      dig
      fastfetch
      fzf
      gnuplot
      go-task
      jankyborders
      #jupyter
      openssh
      pngpaste
      poppler_utils
      python3
      raycast
      ripgrep
      sketchybar
      tree-sitter

      # :o
      gnutls
      coreutils
      findutils
      gnutar
      gnused
      gawk
      getopt
      indent
      gnugrep
    ];
  };

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    bash.enable = true;
    zsh.enable = true;
  };

  homebrew = {
    enable = true;
    onActivation.cleanup = "uninstall";
    taps = [ ];
    brews = [ ];
    casks = [
      "orbstack"
      "tunnelblick"
      "utm"
      "ghostty"
    ];
  };

  system = {
    # Used for backwards compatibility, please read the changelog before changing
    # $ darwin-rebuild changelog
    stateVersion = 4;
    defaults.screencapture.target = "clipboard";
  };

  security.pam.enableSudoTouchIdAuth = true;
}

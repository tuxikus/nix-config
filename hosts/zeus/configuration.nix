{
  config,
  pkgs,
  inputs,
  home-manager,
  ...
}:
let
  nixModulesDirectory = ../../modules/nixos;

  customPackages = pkgs: {
    retroarch-joypad-autoconfig = pkgs.retroarch-joypad-autoconfig.overrideAttrs {
      src = pkgs.fetchFromGitHub {
        owner = "tuxikus";
        repo = "retroarch-joypad-autoconfig";
        rev = "70ee2f01584891f65e380cf1976a2a980d984960";
        hash = "sha256-v3Ocw7bksCuhdOy/ec+a5Mo6yuNwNQmjuOFirc7Eo0Y=";
      };
    };
  };
in
{

  imports = [
    ./hardware-configuration.nix
    (nixModulesDirectory + "/udev/zsa-keyboards.nix")
    (nixModulesDirectory + "/udev/vial.nix")
    (nixModulesDirectory + "/virtualization.nix")
    (nixModulesDirectory + "/podman.nix")
    (nixModulesDirectory + "/development/c.nix")
    (nixModulesDirectory + "/development/python.nix")
    (nixModulesDirectory + "/development/nix.nix")
  ];

  nix = {
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  boot.loader = {
    systemd-boot.enable = false;
    grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
    };
    efi.canTouchEfiVariables = true;
  };

  networking.hostName = "zeus";
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Berlin";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };
  };

  security.rtkit.enable = true;

  users = {
    groups = {
      tuxikus = { };
      plugdev = { }; # group to flash zsa moonlander mk1 keyboard with oryx in chromium
    };
    users = {
      tuxikus = {
        uid = 1000;
        isNormalUser = true;
        description = "tuxikus";
        group = "tuxikus";
        extraGroups = [
          "networkmanager"
          "wheel"
          "plugdev"
        ];
      };
    };
  };

  fonts.packages = with pkgs; [
    nerdfonts
    profont
  ];

  services = {
    dbus.enable = true;
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      windowManager.qtile = {
        enable = true;
        extraPackages =
          python3Packages: with python3Packages; [
            qtile-extras
          ];
      };
      xkb = {
        layout = "us";
        variant = "";
      };
    };
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    mpd = {
      enable = true;
      musicDirectory = "/home/tuxikus/multimedia/music/mp3";
      extraConfig = ''
        	audio_output {
        	 type "pipewire"
        	 name "My PipeWire Output"
        	}
      '';
      #network.startWhenNeeded = true;
      user = "tuxikus";
    };
  };
  environment.systemPackages = with pkgs; [
    chromium
    tree
    home-manager
    pavucontrol
    wget
    hyprpaper
    hyprsunset
    mpd
    ncmpcpp
    fuzzel
    dunst
    waybar
    grim
    slurp
    bat
    ripgrep
    fzf
    fastfetch
    keepassxc
    unzip
    mpv
    calibre
    direnv
    tree-sitter
    ghostty
    ffmpeg
    yt-dlp
    dig
    vial
    nyxt
    qutebrowser
    wlr-randr
    ripgrep
    samba
    cifs-utils
    platformio
    (customPackages pkgs).retroarch-joypad-autoconfig
    (retroarch.override {
      cores = with libretro; [
        nestopia
        mupen64plus
      ];
    })
  ];

  programs = {
    hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    ssh.startAgent = true;
  };

  systemd.services.mpd.environment = {
    #XDG_RUNTIME_DIR = "/run/user/${toString config.users.users.tuxikus.uid}";
    XDG_RUNTIME_DIR = "/run/user/1000";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

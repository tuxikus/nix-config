{
  config,
  pkgs,
  inputs,
  home-manager,
  ...
}:
let
  nixModulesDirectory = ../../modules/nixos;
in
{

  imports = [
    ./hardware-configuration.nix
    (nixModulesDirectory + "/udev/zsa-keyboards.nix")
    (nixModulesDirectory + "/udev/vial.nix")
    (nixModulesDirectory + "/udev/platformio.nix")
    (nixModulesDirectory + "/virtualization.nix")
    (nixModulesDirectory + "/podman.nix")
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
    pcscd.enable = true;
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
  };
  environment.systemPackages = with pkgs; [
    chromium
    firefox
    tree
    home-manager
    pavucontrol
    wget
    hyprpaper
    hyprsunset
    mpd
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
    tree-sitter
    ghostty
    ffmpeg
    yt-dlp
    dig
    vial
    wlr-randr
    ripgrep
    samba
    cifs-utils
    platformio
    arduino-ide
    python3
    yubioath-flutter
  ];

  programs = {
    gnupg.agent = {
      enable = true;
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    ssh.startAgent = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

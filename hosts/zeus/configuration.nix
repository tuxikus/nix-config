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
    (nixModulesDirectory + "/virt.nix")
    (nixModulesDirectory + "/podman.nix")
  ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    efiSupport = true;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zeus";
  networking.networkmanager.enable = true;

  services.dbus.enable = true;

  time.timeZone = "Europe/Berlin";

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
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


  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;


  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  #services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  #######################################
  ### Groups
  # group of main user
  users.groups.tuxikus = { };
  # group to flash zsa moonlander mk1 keyboard with oryx in chromium
  users.groups.plugdev = { };
  #######################################
  ### Users
  # main user
  users.users.tuxikus = {
    isNormalUser = true;
    description = "tuxikus";
    group = "tuxikus";
    extraGroups = [
      "networkmanager"
      "wheel"
      "plugdev"
    ];
  };
  ########################################

  fonts.packages = with pkgs; [
    nerdfonts
  ];

  environment.systemPackages = with pkgs; [
    firefox
    chromium
    home-manager
    pavucontrol
    python3
    foot
    wget
    hyprpaper
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
    dex
  ];

  # hyprland
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # ssh
  programs.ssh.startAgent = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

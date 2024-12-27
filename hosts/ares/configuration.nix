{
  config,
  pkgs,
  inputs,
  ...
}:
let
  nixModulesDirectory = ../../modules/nixos;
in
{
  imports = [
    ./hardware-configuration.nix
  ];

  # enable nix flakes
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

  networking.hostName = "ares"; # Define your hostname.
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

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  #######################################
  ### Groups
  # group of main user
  users.groups.tux = { };
  # group of gaming user
  users.groups.tux-gaming = { };
  #######################################
  ### Users
  # main user
  users.users.tux = {
    isNormalUser = true;
    description = "tux";
    group = "tux";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };
  # gaming user
  users.users.tux-gaming = {
    isNormalUser = true;
    description = "tux-gaming";
    group = "tux-gaming";
  };
  ########################################

  environment.systemPackages = with pkgs; [
    firefox
    chromium
    flatpak
  ];

  ### flatpak config
  services.flatpak.enable = true;
  services.flatpak.packages = [
    "com.valvesoftware.Steam"
    "com.github.tchx84.Flatseal"
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

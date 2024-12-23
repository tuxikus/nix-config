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

  # enable nix flakes
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # no need no to change nixos-config path if using flakes :)
  #nix.nixPath = [
    #  "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    #  "nixos-config=/home/tuxikus/.nix-config/hosts/zeus/configuration.nix"
    #  "/nix/var/nix/profiles/per-user/root/channels"
    #];

    # Bootloader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "zeus"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Enable networking
    networking.networkmanager.enable = true;

    services.dbus.enable = true;

    # Set your time zone.
    time.timeZone = "Europe/Berlin";

    # Select internationalisation properties.
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

    # Enable the X11 windowing system.
    services.xserver.enable = true;

    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;

    # Configure keymap in X11
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
      btop
      fastfetch
      keepassxc
      unzip
      mpv
      xfce.thunar
    ];

    # hyprland
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    # ssh
    programs.ssh.startAgent = true;

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
      #   enable = true;
      #   enableSSHSupport = true;
      # };

      # List services that you want to enable:

      # Enable the OpenSSH daemon.
      # services.openssh.enable = true;

      # Open ports in the firewall.
      # networking.firewall.allowedTCPPorts = [ ... ];
      # networking.firewall.allowedUDPPorts = [ ... ];
      # Or disable the firewall altogether.
      # networking.firewall.enable = false;

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "24.05"; # Did you read the comment?
}

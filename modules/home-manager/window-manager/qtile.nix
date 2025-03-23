{ config, lib, ... }:
{
  options = {
    qtileWallpaper = lib.mkOption {
      type = lib.types.path;
    };
  };
  config = {
    home.file = {
      ".config/qtile/config.py".text = ''
      
      '';

      ".config/qtile/autostart.sh" = {
        text = ''
          #!/usr/bin/env sh
          wlr-randr --output DP-3 --mode 2560x1440@144 &
        '';
        executable = true;
      };
    };
  };
}

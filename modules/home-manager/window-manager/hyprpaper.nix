{
  config,
  pkgs,
  lib,
  ...
}:
let
  hyprConfigDirectory = ".config/hypr";
in
{
  options = {
    wallpaper = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = {
    home.file."${hyprConfigDirectory}/hyprpaper.conf".text = ''
      preload = ${config.wallpaper}
      wallpaper = DP-3, ${config.wallpaper}
    '';
  };
}

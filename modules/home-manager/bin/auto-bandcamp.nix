{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file."temp/bandcamp-downloads/auto-bandcamp" = {
    source = ./scripts/auto-bandcamp.py;
    executable = true;
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".local/bin/home-backup" = {
    source = ./scripts/home-backup.sh;
    executable = true;
  };
}

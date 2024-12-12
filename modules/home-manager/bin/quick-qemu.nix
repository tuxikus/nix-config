{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".local/bin/quick-qemu" = {
    source = ./scripts/quick-qemu.sh;
    executable = true;
  };
}

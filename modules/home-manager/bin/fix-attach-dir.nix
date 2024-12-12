{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.file.".local/bin/fix-attach-dir" = {
    source = ./scripts/fix-attach-dir.py;
    executable = true;
  };
}

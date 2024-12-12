{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    # formatter
    nixfmt-rfc-style
    # lsp
    nixd
  ];
}

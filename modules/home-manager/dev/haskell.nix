{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    # compiler
    ghc
    # lsp
    haskell-language-server
    # builder
    cabal-install
  ];
}

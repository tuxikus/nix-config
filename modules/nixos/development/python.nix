{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    python312Full
    pyright
  ];
}

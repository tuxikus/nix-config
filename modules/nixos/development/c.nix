{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gcc
    clang
    clang-tools
    cmake
  ];
}

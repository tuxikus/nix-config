{ config, pkgs, ... }:
{
  environment = {
    systemPackages = [ pkgs.qemu ];
  };

  programs.virt-manager.enable = true;
}

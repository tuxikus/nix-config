{ config, pkgs, ... }:
{
  environment = {
    systemPackages = [ pkgs.qemu pkgs.OVMFFull ];
  };

  programs.virt-manager.enable = true;
}

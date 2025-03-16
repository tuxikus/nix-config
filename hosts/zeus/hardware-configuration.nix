{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
imports = [
  (modulesPath + "/installer/scan/not-detected.nix")
];
boot = {
  initrd = {
    availableKernelModules = [
	"nvme"
	"xhci_pci"
	"ahci"
	"usbhid"
	"uas"
	"sd_mod"
    ];
    kernelModules = [];
  };
  kernelModules = [ "kvm-amd" ];
  extraModulePackages = [];
};
hardware = {
  pulseaudio.enable = false;
  cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
};
networking.useDHCP = lib.mkDefault true;


  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}

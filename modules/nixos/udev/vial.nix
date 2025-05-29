{
  pkgs,
  ...
}:
{
  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "udev-file";
      text = ''
        KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="tuxikus", TAG+="uaccess", TAG+="udev-acl"
      '';
      destination = "/etc/udev/rules.d/99-vial.rules";
    })
  ];
}

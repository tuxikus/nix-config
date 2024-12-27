{
  disko.devices = {
    disk = {
      nixos-disk = {
        device = "/dev/disk/by-id/nvme-Samsung_SSD_970_EVO_Plus_500GB_S4EVNM0T907399L";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "500M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
      games-disk = {
        device = "/dev/disk/by-id/nvme-SAMSUNG_MZVLB1T0HBLR_000L2_S4DZNF0N620723";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            games = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/mnt/games";
              };
            };
          };
        };
      };
    };
  };
}

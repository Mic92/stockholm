{ lib, disk, keyFile, ... }:
{
  disk = {
    main = {
      type = "disk";
      device = disk;
      content = {
        type = "table";
        format = "gpt";
        partitions = [
          {
            name = "boot";
            type = "partition";
            start = "0";
            end = "1M";
            part-type = "primary";
            flags = ["bios_grub"];
          }
          {
            type = "partition";
            name = "ESP";
            start = "1MiB";
            end = "50%";
            fs-type = "fat32";
            bootable = true;
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          }
          {
            name = "root";
            type = "partition";
            start = "50%";
            end = "100%";
            content = {
              type = "luks";
              name = "hilum_luks";
              keyFile = keyFile;
              content = {
                type = "filesystem";
                format = "xfs";
                mountpoint = "/";
              };
            };
          }
        ];
      };
    };
  };
}


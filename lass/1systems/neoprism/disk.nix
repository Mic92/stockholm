{ lib, ... }:
{
  disk = (lib.genAttrs [ "/dev/nvme0n1" "/dev/nvme1n1" ] (disk: {
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
          start = "1M";
          end = "1GiB";
          fs-type = "fat32";
          bootable = true;
          content = {
            type = "mdraid";
            name = "boot";
          };
        }
        {
          type = "partition";
          name = "zfs";
          start = "1GiB";
          end = "100%";
          content = {
            type = "zfs";
            pool = "zroot";
          };
        }
      ];
    };
  })) // {
    hdd1 = {
      type = "disk";
      device = "/dev/sda";
      content = {
        type = "zfs";
        pool = "tank";
      };
    };
  };
  mdadm = {
    boot = {
      type = "mdadm";
      level = 1;
      metadata = "1.0";
      content = {
        type = "filesystem";
        format = "vfat";
        mountpoint = "/boot";
      };
    };
  };
  zpool = {
    zroot = {
      type = "zpool";
      mode = "mirror";
      mountpoint = "/";
      rootFsOptions = {
      };
      datasets.reserved = {
        zfs_type = "filesystem";
        options.refreservation = "1G";
      };
    };
    tank = {
      type = "zpool";
      datasets = {
        reserved = {
          zfs_type = "filesystem";
          options.refreservation = "1G";
        };
        containers = {
          zfs_type = "filesystem";
          mountpoint = "/var/lib/containers";
        };
        home = {
          zfs_type = "filesystem";
          mountpoint = "/home";
        };
        srv = {
          zfs_type = "filesystem";
          mountpoint = "/srv";
        };
        libvirt = {
          zfs_type = "filesystem";
          mountpoint = "/var/lib/libvirt";
        };
        # encrypted = {
        #   zfs_type = "filesystem";
        #   options = {
        #     mountpoint = "none";
        #     encryption = "aes-256-gcm";
        #     keyformat = "passphrase";
        #     keylocation = "prompt";
        #   };
        # };

        # "encrypted/download" = {
        #   zfs_type = "filesystem";
        #   mountpoint = "/var/download";
        # };
      };
    };
  };
}

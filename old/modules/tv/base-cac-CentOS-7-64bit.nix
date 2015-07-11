{ config, pkgs, ... }:

{
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  boot.initrd.availableKernelModules = [
    "ata_piix"
    "vmw_pvscsi"
  ];

  fileSystems."/" = {
    device = "/dev/centos/root";
    fsType = "xfs";
  };

  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "xfs";
  };

  swapDevices = [
    { device = "/dev/centos/swap"; }
  ];
}


with import ./lib;
{ config, ... }: {
  imports = [
    ../..
    ../../2configs/hw/winmax2.nix
    ../../2configs/retiolum.nix
    ../../2configs/wiregrill.nix
  ];

  boot.initrd.luks.devices.main.device = "/dev/nvme0n1p2";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  fileSystems."/" = {
    device = "main/root";
    fsType = "zfs";
  };
  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };
  fileSystems."/home" = {
    device = "main/home";
    fsType = "zfs";
  };
  fileSystems."/bku" = {
    device = "main/bku";
    fsType = "zfs";
  };

  krebs.build.host = config.krebs.hosts.ru;

  system.stateVersion = "22.11";
}

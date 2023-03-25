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
    device = "/dev/mapper/ruvg0-root";
    fsType = "btrfs";
    options = ["defaults" "noatime" "compress=zstd"];
  };
  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };
  fileSystems."/home" = {
    device = "/dev/mapper/ruvg0-home";
    fsType = "btrfs";
    options = ["defaults" "noatime" "compress=zstd"];
  };
  fileSystems."/bku" = {
    device = "/dev/mapper/ruvg0-bku";
    fsType = "btrfs";
    options = ["defaults" "noatime" "compress=zstd"];
  };

  krebs.build.host = config.krebs.hosts.ru;

  system.stateVersion = "22.11";
}

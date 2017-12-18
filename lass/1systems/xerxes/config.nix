{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/gpd-pocket.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
  ];

  krebs.build.host = config.krebs.hosts.xerxes;

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="b0:f1:ec:9f:5c:78", NAME="wl0"
  '';

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/d227d88f-bd24-4e8a-aa14-9e966b471437";
    fsType = "btrfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/16C8-D053";
    fsType = "vfat";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/1ec4193b-7f41-490d-8782-7677d437b358";
    fsType = "btrfs";
  };

  boot.initrd.luks.devices = [ { name = "luksroot"; device = "/dev/disk/by-uuid/d17f19a3-dcba-456d-b5da-e45cc15dc9c8"; } ];
  networking.wireless.enable = true;
}

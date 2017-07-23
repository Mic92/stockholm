{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/coreboot.nix>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/backups.nix>
    <stockholm/lass/2configs/games.nix>
  ];

  krebs.build.host = config.krebs.hosts.icarus;

  fileSystems = {
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:a0:0c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:71:cb:35", NAME="et0"
  '';
}

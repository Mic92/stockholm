{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>

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

  krebs.build.host = config.krebs.hosts.daedalus;

  fileSystems = {
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:e8:c8", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:8f:8a:78", NAME="et0"
  '';
}

with import <stockholm/lib>;
{ config, pkgs, ... }: {

  krebs.build.host = config.krebs.hosts.wu;

  imports = [
    <stockholm/tv>
    <stockholm/tv/2configs/hw/w110er.nix>
    <stockholm/tv/2configs/exim-retiolum.nix>
    <stockholm/tv/2configs/pulse.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xserver>
  ];

  boot.initrd.luks.devices.wuca.device = "/dev/sda2";

  fileSystems = {
    "/" = {
      device = "/dev/mapper/wuvga-root";
      fsType = "ext4";
    };
    "/bku" = {
      device = "/dev/mapper/wuvga-bku";
      fsType = "ext4";
    };
    "/home" = {
      device = "/dev/mapper/wuvga-home";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/sda1";
    };
  };

  networking.wireless.enable = true;
  networking.wireless.interfaces = [
    "wlp3s0"
  ];
  networking.interfaces.enp4s0f2.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.useDHCP = false;

}

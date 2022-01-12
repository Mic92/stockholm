{ config, ... }: {
  imports = [
    ./disks.nix
    <stockholm/tv>
    <stockholm/tv/2configs/hw/x220.nix>
    <stockholm/tv/2configs/ppp.nix>
    <stockholm/tv/2configs/retiolum.nix>
    <stockholm/tv/2configs/xsessions>
  ];

  krebs.build.host = config.krebs.hosts.au;

  networking.wireless.enable = true;
  networking.useDHCP = false;
  networking.interfaces.enp0s25.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.interfaces.wwp0s29u1u4i6.useDHCP = true;

  system.stateVersion = "20.03";

  tv.hw.screens.primary.width = 1920;
  tv.hw.screens.primary.height = 1080;
}

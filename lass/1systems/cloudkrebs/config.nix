{ config, lib, pkgs, ... }:

let
  inherit (import <stockholm/lass/4lib> { inherit pkgs lib; }) getDefaultGateway;
  ip = config.krebs.build.host.nets.internet.ip4.addr;
in {
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/os-templates/CAC-CentOS-7-64bit.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/realwallpaper.nix>
    <stockholm/lass/2configs/privoxy-retiolum.nix>
    {
      networking.interfaces.enp2s1.ip4 = [
        {
          address = ip;
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = getDefaultGateway ip;
      networking.nameservers = [
        "8.8.8.8"
      ];

    }
    {
      sound.enable = false;
    }
  ];

  krebs.build.host = config.krebs.hosts.cloudkrebs;
}

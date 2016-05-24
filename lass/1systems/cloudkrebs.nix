{ config, lib, pkgs, ... }:

let
  inherit (import ../4lib { inherit pkgs lib; }) getDefaultGateway;

  ip = config.krebs.build.host.nets.internet.ip4.addr;
in {
  imports = [
    ../.
    ../2configs/os-templates/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    ../2configs/retiolum.nix
    ../2configs/fastpoke-pages.nix
    ../2configs/git.nix
    ../2configs/realwallpaper.nix
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

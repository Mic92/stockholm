{ config, lib, pkgs, ... }:

let
  inherit (lib) head;

  ip = "168.235.145.85";
  gw = "168.235.145.1";
in {
  imports = [
    ../2configs/base.nix
    ../2configs/os-templates/CAC-CentOS-7-64bit.nix
    {
      networking.interfaces.enp2s1.ip4 = [
        {
          address = ip;
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = gw;
      networking.nameservers = [
        "8.8.8.8"
      ];

    }
    {
      sound.enable = false;
    }
  ];

  krebs.build.host = config.krebs.hosts.test-centos7;
}

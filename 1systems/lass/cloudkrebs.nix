{ config, pkgs, ... }:

{
  imports = [
    ../../2configs/tv/CAC-Developer-2.nix
    ../../2configs/tv/CAC-CentOS-7-64bit.nix
    ../../2configs/lass/base.nix
    ../../2configs/lass/retiolum.nix
    ../../2configs/lass/fastpoke-pages.nix
    {
      networking.interfaces.enp2s1.ip4 = [
        {
          address = "104.167.113.104";
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = "104.167.113.1";
      networking.nameservers = [
        "8.8.8.8"
      ];

    }
    {
      imports = [ ../../3modules/tv/identity.nix ];
      tv.identity = {
        enable = true;
      };
    }
  ];

  networking.hostName = "cloudkrebs";

}

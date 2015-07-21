{ config, pkgs, ... }:

{
  imports = [
    ../../2configs/tv/CAC-Developer-2.nix
    ../../2configs/tv/CAC-CentOS-7-64bit.nix
    ../../2configs/lass/base.nix
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
      imports = [ ../../3modules/tv/retiolum.nix ];
      tv.retiolum = {
        enable = true;
        hosts = ../../Zhosts;
        connectTo = [
          "fastpoke"
          "gum"
          "pigstarter"
        ];
      };
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

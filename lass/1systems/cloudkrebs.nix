{ config, pkgs, ... }:

{
  imports = [
    ../../tv/2configs/CAC-Developer-2.nix
    ../../tv/2configs/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    ../2configs/retiolum.nix
    ../2configs/fastpoke-pages.nix
    ../2configs/new-repos.nix
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
  ];

  krebs.build = {
    user = config.krebs.users.lass;
    target = "root@cloudkrebs";
    host = config.krebs.hosts.cloudkrebs;
    deps = {
      secrets = {
        url = "/home/lass/secrets/${config.krebs.build.host.name}";
      };
      stockholm = {
        url = toString ../..;
      };
    };
  };

  networking.hostName = "cloudkrebs";

}

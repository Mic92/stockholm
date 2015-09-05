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
          address = "162.248.167.198";
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = "162.248.167.1";
      networking.nameservers = [
        "8.8.8.8"
      ];

    }
  ];

  krebs.build = {
    user = config.krebs.users.lass;
    target = "root@162.248.167.198";
    host = config.krebs.hosts.echelon;
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

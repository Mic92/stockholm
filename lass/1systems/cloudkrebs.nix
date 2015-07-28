{ config, pkgs, ... }:

{
  imports = [
    ../../2configs/tv/CAC-Developer-2.nix
    ../../2configs/tv/CAC-CentOS-7-64bit.nix
    ../../2configs/lass/base.nix
    ../../2configs/lass/retiolum.nix
    ../../2configs/lass/fastpoke-pages.nix
    ../../2configs/lass/new-repos.nix
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
    target = "root@cloudkrebs";
    host = config.krebs.hosts.cloudkrebs;
    deps = {
      nixpkgs = {
        url = https://github.com/Lassulus/nixpkgs;
        rev = "1879a011925c561f0a7fd4043da0768bbff41d0b";
      };
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

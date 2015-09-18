{ config, lib, pkgs, ... }:

let
  inherit (import ../4lib { inherit pkgs lib; }) getDefaultGateway;
  inherit (lib) head;

  ip = (head config.krebs.hosts.echelon.nets.internet.addrs4);
in {
  imports = [
    ../../tv/2configs/CAC-Developer-2.nix
    ../../tv/2configs/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    ../2configs/retiolum.nix
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
  ];

  krebs.build = {
    user = config.krebs.users.lass;
    target = "root@${ip}";
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

{ config, lib, pkgs, ... }:

let
  inherit (import ../4lib { inherit pkgs lib; }) getDefaultGateway;
  inherit (lib) head;

  ip = (head config.krebs.build.host.nets.internet.addrs4);
in {
  imports = [
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
      nix.maxJobs = 1;
      sound.enable = false;
    }
  ];

  krebs.build = {
    user = config.krebs.users.lass;
    host = config.krebs.hosts.cloudkrebs;
    source = {
      dir.secrets = {
        host = config.krebs.hosts.mors;
        path = "/home/lass/secrets/${config.krebs.build.host.name}";
      };
      dir.stockholm = {
        host = config.krebs.hosts.mors;
        path = "/home/lass/dev/stockholm";
      };
    };
  };

  networking.hostName = "cloudkrebs";

  environment.systemPackages = [
    pkgs.dic
  ];

}

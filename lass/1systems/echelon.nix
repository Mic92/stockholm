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
    ../2configs/realwallpaper-server.nix
    ../2configs/privoxy-retiolum.nix
    ../2configs/git.nix
    ../2configs/redis.nix
    ../2configs/go.nix
    ../2configs/ircd.nix
    ../2configs/newsbot-js.nix
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
    {
      imports = [
        ../3modules/dnsmasq.nix
      ];
      lass.dnsmasq = {
        enable = true;
        config = ''
          interface=retiolum
        '';
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-i retiolum -p udp --dport 53"; target = "ACCEPT"; }
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.echelon;
}

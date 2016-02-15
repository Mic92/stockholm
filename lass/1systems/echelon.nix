{ config, lib, pkgs, ... }:

let
  inherit (import ../4lib { inherit pkgs lib; }) getDefaultGateway;
  inherit (lib) head;

  ip = (head config.krebs.build.host.nets.internet.addrs4);
in {
  imports = [
    ../.
    ../2configs/os-templates/CAC-CentOS-7-64bit.nix
    ../2configs/base.nix
    ../2configs/retiolum.nix
    ../2configs/realwallpaper-server.nix
    ../2configs/privoxy-retiolum.nix
    ../2configs/git.nix
    #../2configs/redis.nix
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
    {
      users.extraUsers = {
        satan = {
          name = "satan";
          uid = 1338;
          home = "/home/satan";
          group = "users";
          createHome = true;
          useDefaultShell = true;
          extraGroups = [
          ];
          openssh.authorizedKeys.keys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+l3ajjOd80uJBM8oHO9HRbtA5hK6hvrpxxnk7qWW7OloT9IXcoM8bbON755vK0O6XyxZo1JZ1SZ7QIaOREGVIRDjcbJbqD3O+nImc6Rzxnrz7hvE+tuav9Yylwcw5HeQi82UIMGTEAwMHwLvsW6R/xyMCuOTbbzo9Ib8vlJ8IPDECY/05RhL7ZYFR0fdphI7jq7PobnO8WEpCZDhMvSYjO9jf3ac53wyghT3gH7AN0cxTR9qgQlPHhTbw+nZEI0sUKtrIhjfVE80wgK3NQXZZj7YAplRs/hYwSi7i8V0+8CBt2epc/5RKnJdDHFQnaTENq9kYQPOpUCP6YUwQIo8X nineinchnade@gmail.com"
          ];
        };
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.echelon;
}

{ config, lib, ... }:

let
  r_ip = (head config.krebs.build.host.nets.retiolum.addrs4);
  inherit (lib) head;

in {
  imports = [
    ./privoxy.nix
  ];

  services.privoxy.listenAddress = "${r_ip}:8118";

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-i retiolum -p tcp --dport 8118"; target = "ACCEPT"; }
      ];
    };
  };
}

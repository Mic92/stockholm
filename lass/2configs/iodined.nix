{ pkgs, config, ... }:

let
  # TODO: make this a parameter
  domain = "io.lassul.us";
  pw = import <secrets/iodinepw.nix>;
in {

  services.iodined = {
    enable = true;
    domain = domain;
    ip = "172.16.10.1/24";
    extraConfig = "-c -P ${pw} -l ${config.krebs.build.host.nets.internet.ip4.addr}";
  };

  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p udp --dport 53"; target = "ACCEPT";}
  ];

}

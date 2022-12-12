with import ./lib;
{ config, ... }: let
  cfg.host = config.krebs.build.host;
in {
  services.openssh = {
    enable = true;
  };
  tv.iptables.input-internet-accept-tcp = singleton "ssh";
  tv.iptables.extra.nat.OUTPUT = [
    "-o lo -p tcp --dport 11423 -j REDIRECT --to-ports 22"
  ];
  tv.iptables.extra4.nat.PREROUTING = [
    "-d ${cfg.host.nets.retiolum.ip4.addr} -p tcp --dport 22 -j ACCEPT"
  ];
  tv.iptables.extra6.nat.PREROUTING = [
    "-d ${cfg.host.nets.retiolum.ip6.addr} -p tcp --dport 22 -j ACCEPT"
  ];
  tv.iptables.extra.nat.PREROUTING = [
    "-p tcp --dport 22 -j REDIRECT --to-ports 0"
    "-p tcp --dport 11423 -j REDIRECT --to-ports 22"
  ];
}

with import ./lib;
{ config, ... }: let
  cfg.host = config.krebs.build.host;
  nets =
    optional (cfg.host.nets?retiolum) cfg.host.nets.retiolum ++
    optional (cfg.host.nets?wiregrill) cfg.host.nets.wiregrill;
in {
  services.openssh = {
    enable = true;
  };
  tv.iptables.input-internet-accept-tcp = singleton "ssh";
  tv.iptables.extra.nat.OUTPUT = [
    "-o lo -p tcp --dport 11423 -j REDIRECT --to-ports 22"
  ];
  tv.iptables.extra4.nat.PREROUTING =
    map
      (net: "-d ${net.ip4.addr} -p tcp --dport 22 -j ACCEPT")
      (filter (net: net.ip4 != null) nets);
  tv.iptables.extra6.nat.PREROUTING =
    map
      (net: "-d ${net.ip6.addr} -p tcp --dport 22 -j ACCEPT")
      (filter (net: net.ip6 != null) nets);
  tv.iptables.extra.nat.PREROUTING = [
    "-p tcp --dport 22 -j REDIRECT --to-ports 0"
    "-p tcp --dport 11423 -j REDIRECT --to-ports 22"
  ];
}

{ config, lib, pkgs, ... }:
{
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport 9100 -s ${config.krebs.hosts.prism.nets.retiolum.ip4.addr}"; target = "ACCEPT"; v6 = false; }
    { predicate = "-i retiolum -p tcp --dport 9100 -s ${config.krebs.hosts.prism.nets.retiolum.ip6.addr}"; target = "ACCEPT"; v4 = false; }
  ];
  services.prometheus.exporters = {
    node = {
      enable = true;
      enabledCollectors = [
        "systemd"
      ];
    };
  };
}

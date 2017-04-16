{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.syncthing = {
    enable = true;
    useInotify = true;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];
}

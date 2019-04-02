{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.syncthing = {
    enable = true;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];
  krebs.syncthing = {
    enable = true;
    cert = toString <secrets/syncthing.cert>;
    key = toString <secrets/syncthing.key>;
    peers = mapAttrs (n: v: { id = v.syncthing.id; }) (filterAttrs (n: v: v.syncthing.id != null) config.krebs.hosts);
    folders = [
      { path = "/tmp/testsync"; peers = [ "icarus" "mors" "skynet" ]; }
    ];
  };
}

{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  services.syncthing = {
    enable = true;
    group = "syncthing";
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
      { path = "/home/lass/sync"; peers = [ "icarus" "mors" "skynet" "blue" "green" "littleT" "prism"]; }
    ];
  };

  system.activationScripts.syncthing-home = ''
    ${pkgs.coreutils}/bin/chmod a+x /home/lass
  '';

  lass.ensure-permissions = [
    { folder = "/home/lass/sync"; owner = "lass"; group = "syncthing"; }
  ];
}

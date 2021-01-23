{ config, pkgs, ... }: with import <stockholm/lib>; let
  all_peers = filterAttrs (n: v: v.syncthing.id != null) config.krebs.hosts;
  mk_peers = mapAttrs (n: v: { id = v.syncthing.id; });
in {
  services.syncthing = {
    enable = true;
    group = "syncthing";
    configDir = "/var/lib/syncthing";
    declarative = {
      key = toString <secrets/syncthing.key>;
      cert = toString <secrets/syncthing.cert>;
      devices = mk_peers all_peers;
    };
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];

  system.activationScripts.syncthing-home = ''
    ${pkgs.coreutils}/bin/chmod a+x /home/lass
  '';

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
}

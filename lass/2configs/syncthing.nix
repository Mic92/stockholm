{ config, pkgs, ... }: with import <stockholm/lib>; let
  all_peers = filterAttrs (n: v: v.syncthing.id != null) config.krebs.hosts;
  own_peers = filterAttrs (n: v: v.owner.name == "lass") all_peers;
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
      folders."/home/lass/sync" = {
        devices = attrNames (filterAttrs (n: v: n != "phone") own_peers);
        # ignorePerms = false;
      };
    };
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport 22000"; target = "ACCEPT";}
    { predicate = "-p udp --dport 21027"; target = "ACCEPT";}
  ];

  system.activationScripts.syncthing-home = ''
    ${pkgs.coreutils}/bin/chmod a+x /home/lass
  '';

  krebs.permown."/home/lass/sync" = {
    file-mode = "u+rw,g+rw";
    owner = "lass";
    group = "syncthing";
    umask = "0002";
    keepGoing = true;
  };
}

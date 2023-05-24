{ options, config, pkgs, ... }: with import <stockholm/lib>; let
  mk_peers = mapAttrs (n: v: { id = v.syncthing.id; });

  all_peers = filterAttrs (n: v: v.syncthing.id != null) config.krebs.hosts;
  used_peer_names = unique (filter isString (flatten (mapAttrsToList (n: v: v.devices) config.services.syncthing.folders)));
  used_peers = filterAttrs (n: v: elem n used_peer_names) all_peers;
in {
  services.syncthing = {
    enable = true;
    configDir = "/var/lib/syncthing";
    key = toString <secrets/syncthing.key>;
    cert = toString <secrets/syncthing.cert>;
    # workaround for infinite recursion on unstable, remove in 23.11
  } // (if builtins.hasAttr "settings" options.services.syncthing then
    { settings.devices = mk_peers used_peers; }
  else
    { devices = mk_peers used_peers; }
  );

  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
}

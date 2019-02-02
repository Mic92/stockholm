{lib,pkgs, ... }:
let
  pkg = lib.overrideDerivation pkgs.ympd (old: {
      patches = [ ./ympd-top-next.patch ];
  });
  mpdHost = "mpd.shack";
  ympd = name: port: let
    webPort = 10000 + port;
  in {
    systemd.services."ympd-${name}" = {
      description = "mpd for ${name}";
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStart = "${pkg}/bin/ympd --host ${mpdHost} --port ${toString port} --webport ${toString webPort} --user nobody";
    };
    services.nginx.virtualHosts."mobile.${name}.mpd.shack" = {
      serverAliases = [
        "${name}.mpd.wolf.r"
        "${name}.mpd.wolf.shack"
      ];
      locations."/".proxyPass = "http://localhost:${toString webPort}";
    };
  };
in lib.mkMerge [{
  services.nginx.enable = true;
}
  (ympd "lounge" 6600)
  (ympd "seminarraum" 6601)
  (ympd "elab" 6602)
  (ympd "kueche" 6603)
  (ympd "crafting" 6604)
  (ympd "fablab" 6605)
  (ympd "workshop" 6606)
  (ympd "klo" 6607)

]

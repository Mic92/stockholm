{ config, lib, ... }:

let
  hostname = config.krebs.build.host.name;
  inherit (lib)
    nameValuePair
  ;

in {
  krebs.realwallpaper.enable = true;

  krebs.nginx.servers.wallpaper = {
    server-names = [
      hostname
    ];
    locations = [
      (nameValuePair "/wallpaper.png" ''
        root /tmp/;
      '')
    ];
  };

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; }
      ];
    };
  };
}

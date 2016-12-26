{ config, lib, ... }:

let
  hostname = config.krebs.build.host.name;
  inherit (lib)
    nameValuePair
  ;

in {
  krebs.realwallpaper.enable = true;

  services.nginx.virtualHosts.wallpaper = {
    serverAliases = [
      hostname
    ];
    locations."/wallpaper.png".extraConfig = ''
      root /tmp/;
    '';
  };

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; }
      ];
    };
  };
}

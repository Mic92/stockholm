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
    locations."/realwallpaper.png".extraConfig = ''
      root /var/realwallpaper/;
    '';
    locations."/realwallpaper-sat.png".extraConfig = ''
      root /var/realwallpaper/;
    '';
    locations."/realwallpaper-sat-krebs.png".extraConfig = ''
      root /var/realwallpaper/;
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

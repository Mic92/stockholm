{ config, lib, ... }:

let
  hostname = config.krebs.build.host.name;
  inherit (lib)
    nameValuePair
  ;

in {
  krebs.realwallpaper.enable = true;

  services.nginx.virtualHosts.wallpaper = {
    extraConfig = ''
      if ( $server_addr = "${config.krebs.build.host.nets.internet.ip4.addr}" ) {
        return 403;
      }
    '';
    serverAliases = [
      hostname
      "${hostname}.r"
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

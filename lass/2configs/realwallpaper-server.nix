{ config, lib, ... }:

let
  hostname = config.krebs.build.host.name;
  inherit (lib)
    nameValuePair
  ;

in {
  imports = [
    ./realwallpaper.nix
  ];

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

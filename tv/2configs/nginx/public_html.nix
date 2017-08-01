{ config, lib, ... }:

with import <stockholm/lib>;

{
  services.nginx = {
    enable = true;
    virtualHosts.default = {
      serverAliases = [
        "localhost"
        "${config.krebs.build.host.name}"
        "${config.krebs.build.host.name}.gg23"
        "${config.krebs.build.host.name}.r"
      ];
      locations."~ ^/~(.+?)(/.*)?\$".extraConfig = ''
        alias /home/$1/public_html$2;
      '';
    };
  };
  tv.iptables.input-internet-accept-tcp = singleton "http";
}

{ config, lib, ... }:

with import <stockholm/lib>;

{
  services.nginx = {
    enable = true;
    virtualHosts.default = {
      serverAliases = [
        "localhost"
        "${config.krebs.build.host.name}"
        "${config.krebs.build.host.name}.r"
        "${config.krebs.build.host.name}.retiolum"
      ];
      locations."~ ^/~(.+?)(/.*)?\$".extraConfig = ''
        alias /home/$1/public_html$2;
      '';
    };
  };
  tv.iptables.input-internet-accept-tcp = singleton "http";
}

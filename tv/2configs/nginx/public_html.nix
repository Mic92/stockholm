{ config, lib, ... }:

with import <stockholm/lib>;

{
  krebs.nginx = {
    enable = true;
    servers.default = {
      server-names = [
        "localhost"
        "${config.krebs.build.host.name}"
        "${config.krebs.build.host.name}.r"
        "${config.krebs.build.host.name}.retiolum"
      ];
      locations = [
        (nameValuePair "~ ^/~(.+?)(/.*)?\$" ''
          alias /home/$1/public_html$2;
        '')
      ];
    };
  };
  tv.iptables.input-internet-accept-tcp = singleton "http";
}

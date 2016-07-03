{ config, lib, ... }:

with config.krebs.lib;

{
  krebs.nginx = {
    extraConfig = ''
      events {
      }
    '';
    servers.default.locations = [
      (nameValuePair "= /etc/os-release" ''
        default_type text/plain;
        alias /etc/os-release;
      '')
    ];
  };
  tv.iptables = optionalAttrs config.krebs.nginx.enable {
    input-retiolum-accept-tcp = singleton "http";
  };
}

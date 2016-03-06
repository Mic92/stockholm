{ config, lib, ... }:

with config.krebs.lib;

{
  krebs.nginx = {
    servers.default.locations = [
      (nameValuePair "= /etc/os-release" ''
        default_type text/plain;
        alias /etc/os-release;
      '')
    ];
  };
  tv.iptables = optionalAttrs config.krebs.nginx.enable {
    input-retiolum-accept-new-tcp = singleton "http";
  };
}

{ config, lib, ... }:

with import <stockholm/lib>;

{
  services.nginx = {
    virtualHosts.default = {
      locations."= /etc/os-release".extraConfig = ''
        default_type text/plain;
        alias /etc/os-release;
      '';
    };
  };
  tv.iptables = {
    input-retiolum-accept-tcp = singleton "http";
  };
}

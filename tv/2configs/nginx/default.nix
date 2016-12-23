{ config, lib, ... }:

with import <stockholm/lib>;

{
  services.nginx = {
    virtualHosts._http = {
      default = true;
      extraConfig = ''
        return 404;
      '';
    };

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

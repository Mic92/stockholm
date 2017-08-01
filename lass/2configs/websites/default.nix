{ config, lib, ... }:

with import <stockholm/lib>;

{
  services.nginx = {
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

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
}


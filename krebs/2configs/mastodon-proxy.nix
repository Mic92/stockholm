{ config, lib, pkgs, ... }:
{
  services.nginx = {
    enable = true;
    virtualHosts."social.krebsco.de" = {
      forceSSL = true;
      enableACME = true;
      acmeFallbackHost = "hotdog.r";
      locations."/" = {
        # TODO use this in 22.11
        # recommendedProxySettings = true;
        proxyPass = "http://hotdog.r";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header        Host $host;
          proxy_set_header        X-Real-IP $remote_addr;
          proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header        X-Forwarded-Proto $scheme;
          proxy_set_header        X-Forwarded-Host $host;
          proxy_set_header        X-Forwarded-Server $host;
        '';
      };
    };
  };
}

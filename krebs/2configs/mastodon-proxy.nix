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
        recommendedProxySettings = true;
        proxyPass = "https://hotdog.r";
        proxyWebsockets = true;
      };
    };
  };
}

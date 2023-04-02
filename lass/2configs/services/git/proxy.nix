{ config, pkgs, ... }:
{
  services.nginx.virtualHosts."cgit.lassul.us" = {
    forceSSL = true;
    enableACME = true;
    acmeFallbackHost = "orange.r";
    locations."/" = {
      proxyPass = "http://orange.r";
      proxyWebsockets = true;
      recommendedProxySettings = true;
    };
  };
}

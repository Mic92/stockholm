{ config, pkgs, ... }:
{
  krebs.sync-containers3.containers.orange = {
    sshKey = "${toString <secrets>}/orange.sync.key";
  };
  services.nginx.virtualHosts."lassul.us" = {
    # enableACME = config.security;
    # forceSSL = true;
    locations."/" = {
      recommendedProxySettings = true;
      proxyWebsockets = true;
      proxyPass = "http://orange.r";
    };
  };
}

{ config, pkgs, ... }:
{
  krebs.sync-containers3.containers.ubik = {
    sshKey = "${toString <secrets>}/ubik.sync.key";
  };
  containers.ubik.bindMounts."/var/lib" = {
    hostPath = "/var/lib/sync-containers3/ubik/state";
    isReadOnly = false;
  };
  containers.ubik.bindMounts."/var/lib/nextcloud/data" = {
    hostPath = "/var/ubik";
    isReadOnly = false;
  };
  services.nginx.virtualHosts."c.apanowicz.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      recommendedProxySettings = true;
      proxyWebsockets = true;
      proxyPass = "http://ubik.r";
      extraConfig = ''
        client_max_body_size 9001M;
      '';
    };
  };
}

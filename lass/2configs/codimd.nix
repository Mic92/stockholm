{ config, pkgs, lib, ... }:
with import <stockholm/lib>;
{
  services.nginx.virtualHosts.codimd = {
    enableACME = true;
    addSSL = true;
    serverName = "codi.lassul.us";
    locations."/".extraConfig = ''
      client_max_body_size 4G;
      proxy_set_header Host $host;
      proxy_pass http://localhost:3091;
    '';
  };

  services.hedgedoc = {
    enable = true;
    configuration.allowOrigin = [ "*" ];
    configuration = {
      db = {
        dialect = "sqlite";
        storage = "/var/lib/codimd/db.codimd.sqlite";
        useCDN = false;
      };
      port = 3091;
    };
  };
}


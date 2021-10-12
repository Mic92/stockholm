{ config, pkgs, lib, ... }:
with import <stockholm/lib>;
let
  domain = "codi.lassul.us";
in {
  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "https://localhost:3091";
      proxyWebsockets = true;
    };
  };

  security.acme.certs.${domain}.group = "hedgecert";
  users.groups.hedgecert.members = [ "codimd" "nginx" ];

  security.dhparams = {
    enable = true;
    params.hedgedoc = {};
  };

  services.hedgedoc = {
    enable = true;
    configuration.allowOrigin = [ domain ];
    configuration = {
      db = {
        dialect = "sqlite";
        storage = "/var/lib/codimd/db.codimd.sqlite";
      };
      useCDN = false;
      port = 3091;
      domain = domain;
      allowFreeURL = true;

      useSSL = true;
      protocolUseSSL = true;
      sslCAPath = [ "/etc/ssl/certs/ca-certificates.crt" ];
      sslCertPath = "/var/lib/acme/${domain}/cert.pem";
      sslKeyPath = "/var/lib/acme/${domain}/key.pem";
      dhParamPath = config.security.dhparams.params.hedgedoc.path;
    };
  };
}


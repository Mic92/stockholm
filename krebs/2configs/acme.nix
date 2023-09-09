# generate intermediate certificate with generate-krebs-intermediate-ca
{ config, lib, pkgs, ... }: let
  domain = "ca.r";
in {
  security.acme = {
    acceptTerms = true; # kinda pointless since we never use upstream
    email = "spam@krebsco.de";
    certs.${domain}.server = "https://${domain}:1443/acme/acme/directory"; # use 1443 here cause bootstrapping loop
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    virtualHosts.${domain} = {
      addSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "https://localhost:1443";
      };
      locations."= /ca.crt".alias = ../6assets/krebsAcmeCA.crt;
    };
  };
  krebs.secret.files.krebsAcme = {
    path = "/var/lib/step-ca/intermediate_ca.key";
    owner.name = "root";
    mode = "1444";
    source-path = "${config.krebs.secret.directory}/acme_ca.key";
  };
  services.step-ca = {
    enable = true;
    intermediatePasswordFile = "/dev/null";
    address = "0.0.0.0";
    port = 1443;
    settings = {
      root = pkgs.writeText "root.crt" config.krebs.ssl.rootCA;
      crt = pkgs.writeText "intermediate.crt" config.krebs.ssl.intermediateCA;
      key = "/var/lib/step-ca/intermediate_ca.key";
      dnsNames = [ domain ];
      logger.format = "text";
      db = {
        type = "badger";
        dataSource = "/var/lib/step-ca/db";
      };
      authority = {
        provisioners = [{
          type = "ACME";
          name = "acme";
          forceCN = true;
        }];
        claims = {
          maxTLSCertDuration = "2160h";
          defaultTLSCertDuration = "2160h";
        };
        backdate = "1m0s";
      };
      tls = {
        cipherSuites = [
          "TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256"
          "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256"
        ];
        minVersion = 1.2;
        maxVersion = 1.3;
        renegotiation = false;
      };
    };
  };
}

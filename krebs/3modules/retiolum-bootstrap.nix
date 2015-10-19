{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.krebs.retiolum-bootstrap;

  out = {
    options.krebs.retiolum-bootstrap = api;
    config = mkIf cfg.enable imp ;
  };

  api = {
    enable = mkEnableOption "retiolum boot strap for tinc.krebsco.de";
    hostname = mkOption {
        type = types.str;
        description = "hostname which serves tinc boot";
        default = "tinc.krebsco.de" ;
    };
    ssl_certificate_key = mkOption {
        type = types.str;
        description = "Certificate key to use for ssl";
        default = "/root/secrets/tinc.krebsco.de.key";
    };
    ssl_certificate = mkOption {
        type = types.str;
        description = "Certificate file to use for ssl";
        default = "/root/secrets/tinc.krebsco.de.crt" ;
    };
    # in use:
    #  <secrets/tinc.krebsco.de.crt>
    #  <secrets/tinc.krebsco.de.key>
  };

  imp = {
    # TODO: assert krebs nginx

    krebs.nginx.servers = {
      retiolum-boot-redir = {
        server-names = singleton cfg.hostname;
        extraConfig = ''
          return 301 https://$server_name$request_uri;
        '';
        locations = [];
      };
      retiolum-boot-ssl = {
        server-names = singleton cfg.hostname;
        listen = "443 ssl";
        extraConfig = ''
          ssl_certificate ${cfg.ssl_certificate};
          ssl_certificate_key ${cfg.ssl_certificate_key};
          root ${pkgs.retiolum-bootstrap};
          try_files $uri $uri/retiolum.sh;
        '';
        locations = [];
      };
    };
  };

in
out

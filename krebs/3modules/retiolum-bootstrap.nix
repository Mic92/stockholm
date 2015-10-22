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
    listen = mkOption {
        type = with types; listOf str;
        description = ''Addresses to listen on (nginx-syntax).
        ssl will be configured, http will be redirected to ssl.
        Make sure to have at least 1 ssl port configured.
        '';
        default = [ "80" "443 ssl" ] ;
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
    krebs.nginx.servers = assert config.krebs.nginx.enable; {
      retiolum-boot-ssl = {
        server-names = singleton cfg.hostname;
        listen = cfg.listen;
        extraConfig = ''
          ssl_certificate ${cfg.ssl_certificate};
          ssl_certificate_key ${cfg.ssl_certificate_key};

          if ($scheme = http){
            return 301 https://$server_name$request_uri;
          }

          root ${pkgs.retiolum-bootstrap};
          try_files $uri $uri/retiolum.sh;
        '';
        locations = [];
      };
    };
  };

in
out

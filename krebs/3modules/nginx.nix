{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  cfg = config.krebs.nginx;

  out = {
    options.krebs.nginx = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.nginx";

    default404 = mkOption {
      type = types.bool;
      default = true;
      description = ''
        By default all requests not directed to an explicit hostname are
        replied with a 404 error to avoid accidental exposition of nginx
        services.

        Set this value to `false` to disable this behavior - you will then be
        able to configure a new `default_server` in the listen address entries
        again.
      '';
    };

    servers = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          server-names = mkOption {
            type = with types; listOf str;
            # TODO use identity
            default = [
              "${config.networking.hostName}"
              "${config.networking.hostName}.r"
              "${config.networking.hostName}.retiolum"
            ];
          };
          listen = mkOption {
            type = with types; either str (listOf str);
            default = "80";
            apply = x:
              if typeOf x != "list"
                then [x]
                else x;
          };
          locations = mkOption {
            type = with types; listOf (attrsOf str);
            default = [];
          };
          extraConfig = mkOption {
            type = with types; string;
            default = "";
          };
          ssl = mkOption {
            type = with types; submodule ({
              options = {
                enable = mkEnableOption "ssl";
                certificate = mkOption {
                  type = str;
                };
                certificate_key = mkOption {
                  type = str;
                };
                #TODO: check for valid cipher
                ciphers = mkOption {
                  type = str;
                  default = "AES128+EECDH:AES128+EDH";
                };
                prefer_server_ciphers = mkOption {
                  type = bool;
                  default = true;
                };
                force_encryption = mkOption {
                  type = bool;
                  default = false;
                  description = ''
                    redirect all `http` traffic to the same domain but with ssl
                    protocol.
                  '';
                };
                protocols = mkOption {
                  type = listOf (enum [ "SSLv2" "SSLv3" "TLSv1" "TLSv1.1" "TLSv1.2" ]);
                  default = [ "TLSv1.1" "TLSv1.2" ];

                };
              };
            });
            default = {};
          };
        };
      });
      default = {};
    };
  };

  imp = {
    services.nginx = {
      enable = true;
      httpConfig = ''
        default_type      application/octet-stream;
        sendfile          on;
        keepalive_timeout 65;
        gzip              on;

        ${optionalString cfg.default404 ''
          server {
            listen 80 default_server;
            server_name _;
            return 404;
          }''}

        ${concatStrings (mapAttrsToList (_: to-server) cfg.servers)}
      '';
    };
  };

  indent = replaceChars ["\n"] ["\n  "];

  to-location = { name, value }: ''
    location ${name} {
      ${indent value}
    }
  '';

  to-server = { server-names, listen, locations, extraConfig, ssl, ... }: ''
    server {
      server_name ${toString (unique server-names)};
      ${concatMapStringsSep "\n" (x: indent "listen ${x};") listen}
      ${optionalString ssl.enable (indent ''
        ${optionalString ssl.force_encryption ''
          if ($scheme = http){
            return 301 https://$server_name$request_uri;
          }
        ''}
        listen 443 ssl;
        ssl_certificate ${ssl.certificate};
        ssl_certificate_key ${ssl.certificate_key};
        ${optionalString ssl.prefer_server_ciphers ''
          ssl_prefer_server_ciphers On;
        ''}
        ssl_ciphers ${ssl.ciphers};
        ssl_protocols ${toString ssl.protocols};
      '')}
      ${indent extraConfig}
      ${indent (concatMapStrings to-location locations)}
    }
  '';

in
out

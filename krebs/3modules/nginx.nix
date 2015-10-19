{ config, pkgs, lib, ... }:

with builtins;
with lib;
let
  cfg = config.krebs.nginx;

  out = {
    options.krebs.nginx = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.nginx";

    servers = mkOption {
      type = with types; attrsOf optionSet;
      options = singleton {
        server-names = mkOption {
          type = with types; listOf str;
          # TODO use identity
          default = [
            "${config.networking.hostName}"
            "${config.networking.hostName}.retiolum"
          ];
        };
        listen = mkOption {
          type = with types; str;
          default = "80";
        };
        locations = mkOption {
          type = with types; listOf (attrsOf str);
        };
        extraConfig = mkOption {
          type = with types; str;
          default = "";
        };
      };
      default = {};
    };
  };

  imp = {
    services.nginx = {
      enable = true;
      httpConfig = ''
        include           ${pkgs.nginx}/conf/mime.types;
        default_type      application/octet-stream;
        sendfile          on;
        keepalive_timeout 65;
        gzip              on;
        server {
          listen 80 default_server;
          server_name _;
          return 404;
        }
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

  to-server = { server-names, listen, locations, extraConfig, ... }: ''
    server {
      listen ${listen};
      server_name ${toString server-names};
      ${extraConfig}
      ${indent (concatStrings (map to-location locations))}
    }
  '';

in
out

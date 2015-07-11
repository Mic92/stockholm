{ config, pkgs, lib, ... }:

with builtins;
with lib;
let
  cfg = config.tv.nginx;

  out = {
    options.tv.nginx = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable nginx.";
    };

    retiolum-locations = mkOption {
      type = with types; listOf (attrsOf str);
      default = [];
    };
  };

  imp = {
    services.nginx =
      let
        name = config.tv.retiolum.name;
        qname = "${name}.retiolum";
      in
      assert config.tv.retiolum.enable;
      {
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
            location / {
              return 404;
            }
          }
          server {
            listen 80;
            server_name ${name} ${qname};

            ${indent (concatStrings (map to-location cfg.retiolum-locations))}

            location / {
              return 404;
            }
          }
        '';
      };
  };

  
  indent = replaceChars ["\n"] ["\n  "];

  to-location = { name, value }: ''
    location ${name} {
      ${indent value}
    }
  '';

in
out


#let
#  cfg = config.tv.nginx;
#  arg' = arg // { inherit cfg; };
#in
#
#{
#  options.tv.nginx = import ./options.nix arg';
#  config = lib.mkIf cfg.enable (import ./config.nix arg');
#}

{ cfg, config, lib, pkgs, ... }:

let
  inherit (lib) concatStrings replaceChars;

  indent = replaceChars ["\n"] ["\n  "];

  to-location = { name, value }: ''
    location ${name} {
      ${indent value}
    }
  '';
in

{
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
}

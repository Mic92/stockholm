{ config, pkgs, ... }:

{
  services.nginx =
    let
      name = config.networking.hostName;
      qname = "${name}.retiolum";
    in
      {
        enable = true;
        httpConfig = ''
          sendfile  on;
          server {
            listen      80;
            server_name ${name} ${qname} localhost;
            root /srv/http/${name};
            location ~ ^/~(.+?)(/.*)?$ {
              alias /home/$1/public_html$2;
            }
          }
          types {
            text/css css;
            text/html html;
            image/svg+xml svg;
          }
          default_type text/html;
          charset utf-8;
        '';
      };
}

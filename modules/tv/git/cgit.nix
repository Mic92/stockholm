{ config, lib, pkgs, ... }:

let
  inherit (builtins) attrValues filter getAttr;
  inherit (lib) concatMapStringsSep mkIf optionalString;

  cfg = config.services.git;

  isPublicRepo = getAttr "public"; # TODO this is also in ./default.nix
in

{
  config = mkIf cfg.cgit {

    users.extraUsers = lib.singleton {
      name = "fcgiwrap";
      uid = 2851179180; # genid fcgiwrap
      group = "fcgiwrap";
      home = "/var/empty";
    };

    users.extraGroups = lib.singleton {
      name = "fcgiwrap";
      gid = 2851179180; # genid fcgiwrap
    };

    services.fcgiwrap = {
      enable = true;
      user = "fcgiwrap";
      group = "fcgiwrap";
      # socketAddress = "/run/fcgiwrap.sock" (default)
      # socketType = "unix" (default)
    };

    environment.etc."cgitrc".text = ''
      css=/cgit-static/cgit.css
      logo=/cgit-static/cgit.png

      # if you do not want that webcrawler (like google) index your site
      robots=noindex, nofollow

      virtual-root=/cgit

      # TODO make this nicer
      cache-root=/tmp/cgit

      cache-size=1000
      enable-commit-graph=1
      enable-index-links=1
      enable-index-owner=0
      enable-log-filecount=1
      enable-log-linecount=1
      enable-remote-branches=1

      root-title=public repositories at ${config.networking.hostName}
      root-desc=keep calm and engage

      snapshots=0
      max-stats=year

      ${concatMapStringsSep "\n" (repo: ''
        repo.url=${repo.name}
        repo.path=${cfg.dataDir}/${repo.name}
        ${optionalString (repo.section != null) "repo.section=${repo.section}"}
        ${optionalString (repo.desc != null) "repo.desc=${repo.desc}"}
      '') (filter isPublicRepo (attrValues cfg.repos))}
    '';

    # TODO modular nginx configuration
    services.nginx =
      let
        name = config.networking.hostName;
        qname = "${name}.retiolum";
      in
        {
          enable = true;
          httpConfig = ''
            include           ${pkgs.nginx}/conf/mime.types;
            default_type      application/octet-stream;
            sendfile          on;
            keepalive_timeout 65;
            gzip              on;
            server {
              listen 80;
              server_name ${name} ${qname} localhost;
              root ${pkgs.cgit}/cgit;

              location /cgit-static {
                rewrite ^/cgit-static(/.*)$ $1 break;
                #expires 30d;
              }

              location /cgit {
                include             ${pkgs.nginx}/conf/fastcgi_params;
                fastcgi_param       SCRIPT_FILENAME $document_root/cgit.cgi;
                #fastcgi_param       PATH_INFO       $uri;
                fastcgi_split_path_info ^(/cgit/?)(.+)$;
                fastcgi_param PATH_INFO $fastcgi_path_info;
                fastcgi_param       QUERY_STRING    $args;
                fastcgi_param       HTTP_HOST       $server_name;
                fastcgi_pass        unix:${config.services.fcgiwrap.socketAddress};
              }

              location / {
                return 404;
              }
            }
          '';
        };
  };
}

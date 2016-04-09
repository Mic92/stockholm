{ lib, pkgs, ... }:

with lib;

rec {

  getDefaultGateway = ip:
    concatStringsSep "." (take 3 (splitString "." ip) ++ ["1"]);

  manageCert = domain:
    {
      security.acme = {
        certs."${domain}" = {
          email = "lassulus@gmail.com";
          webroot = "/var/lib/acme/challenges/${domain}";
          plugins = [
            "account_key.json"
            "cert.pem"
            "key.pem"
            "fullchain.pem"
          ];
          group = "nginx";
          allowKeysForGroup = true;
        };
      };

      krebs.nginx.servers."${domain}" = {
        locations = [
          (nameValuePair "/.well-known/acme-challenge" ''
            root /var/lib/acme/challenges/${domain}/;
          '')
        ];
      };
    };

  ssl = domain:
    {
      imports = [
        ( manageCert domain )
        ( activateACME domain )
      ];
    };

  activateACME = domain:
    {
      krebs.nginx.servers."${domain}" = {
        ssl = {
          enable = true;
          certificate = "/var/lib/acme/${domain}/cert.pem";
          certificate_key = "/var/lib/acme/${domain}/key.pem";
        };
      };
    };

  servePage = domain:
    {
      krebs.nginx.servers."${domain}" = {
        server-names = [
          "${domain}"
          "www.${domain}"
        ];
        locations = [
          (nameValuePair "/" ''
            root /srv/http/${domain};
          '')
        ];
      };
    };

  serveOwncloud = domain:
    {
      krebs.nginx.servers."${domain}" = {
        server-names = [
          "${domain}"
          "www.${domain}"
        ];
        locations = [
          (nameValuePair "/" ''
            # The following 2 rules are only needed with webfinger
            rewrite ^/.well-known/host-meta /public.php?service=host-meta last;
            rewrite ^/.well-known/host-meta.json /public.php?service=host-meta-json last;

            rewrite ^/.well-known/carddav /remote.php/carddav/ redirect;
            rewrite ^/.well-known/caldav /remote.php/caldav/ redirect;

            rewrite ^(/core/doc/[^\/]+/)$ $1/index.html;

            try_files $uri $uri/ /index.php;
          '')
          (nameValuePair "~ \.php$" ''
            fastcgi_split_path_info ^(.+\.php)(/.+)$;
            include ${pkgs.nginx}/conf/fastcgi.conf;
            fastcgi_param PATH_INFO $fastcgi_path_info;
            fastcgi_pass unix:/srv/http/${domain}/phpfpm.pool;
          '')
        ];
        extraConfig = ''
          root /srv/http/${domain}/;
          #index index.php;
          access_log /tmp/nginx_acc.log;
          error_log /tmp/nginx_err.log;

          # set max upload size
          client_max_body_size 10G;
          fastcgi_buffers 64 4K;

          rewrite ^/caldav(.*)$ /remote.php/caldav$1 redirect;
          rewrite ^/carddav(.*)$ /remote.php/carddav$1 redirect;
          rewrite ^/webdav(.*)$ /remote.php/webdav$1 redirect;

          error_page 403 /core/templates/403.php;
          error_page 404 /core/templates/404.php;
        '';
      };
      services.phpfpm.poolConfigs."${domain}" = ''
        listen = /srv/http/${domain}/phpfpm.pool
        user = nginx
        group = nginx
        pm = dynamic
        pm.max_children = 5
        pm.start_servers = 2
        pm.min_spare_servers = 1
        pm.max_spare_servers = 3
        listen.owner = nginx
        listen.group = nginx
        # errors to journal
        php_admin_value[error_log] = 'stderr'
        php_admin_flag[log_errors] = on
        catch_workers_output = yes
      '';
    };

}

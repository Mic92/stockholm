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

  manageCerts = domains:
    let
      domain = head domains;
    in {
      security.acme = {
        certs."${domain}" = {
          email = "lassulus@gmail.com";
          webroot = "/var/lib/acme/challenges/${domain}";
          plugins = [
            "account_key.json"
            "key.pem"
            "fullchain.pem"
          ];
          group = "nginx";
          allowKeysForGroup = true;
          extraDomains = genAttrs domains (_: null);
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
          certificate = "/var/lib/acme/${domain}/fullchain.pem";
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
        extraConfig = ''
          # Add headers to serve security related headers
          add_header Strict-Transport-Security "max-age=15768000; includeSubDomains; preload;";
          add_header X-Content-Type-Options nosniff;
          add_header X-Frame-Options "SAMEORIGIN";
          add_header X-XSS-Protection "1; mode=block";
          add_header X-Robots-Tag none;

          # Path to the root of your installation
          root /srv/http/${domain}/;
          # set max upload size
          client_max_body_size 10G;
          fastcgi_buffers 64 4K;

          # Disable gzip to avoid the removal of the ETag header
          gzip off;

          # Uncomment if your server is build with the ngx_pagespeed module
          # This module is currently not supported.
          #pagespeed off;

          index index.php;
          error_page 403 /core/templates/403.php;
          error_page 404 /core/templates/404.php;

          rewrite ^/.well-known/carddav /remote.php/carddav/ permanent;
          rewrite ^/.well-known/caldav /remote.php/caldav/ permanent;

          # The following 2 rules are only needed for the user_webfinger app.
          # Uncomment it if you're planning to use this app.
          rewrite ^/.well-known/host-meta /public.php?service=host-meta last;
          rewrite ^/.well-known/host-meta.json /public.php?service=host-meta-json last;
        '';
        locations = [
          (nameValuePair "/robots.txt" ''
            allow all;
            log_not_found off;
            access_log off;
          '')
          (nameValuePair "~ ^/(build|tests|config|lib|3rdparty|templates|data)/" ''
            deny all;
          '')

          (nameValuePair "~ ^/(?:autotest|occ|issue|indie|db_|console)" ''
            deny all;
          '')

          (nameValuePair "/" ''
            rewrite ^/remote/(.*) /remote.php last;
            rewrite ^(/core/doc/[^\/]+/)$ $1/index.html;
            try_files $uri $uri/ =404;
          '')

          (nameValuePair "~ \.php(?:$|/)" ''
            fastcgi_split_path_info ^(.+\.php)(/.+)$;
            include ${pkgs.nginx}/conf/fastcgi_params;
            fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
            fastcgi_param PATH_INFO $fastcgi_path_info;
            fastcgi_param HTTPS on;
            fastcgi_param modHeadersAvailable true; #Avoid sending the security headers twice
            fastcgi_pass unix:/srv/http/${domain}/phpfpm.pool;
            fastcgi_intercept_errors on;
          '')

          # Adding the cache control header for js and css files
          # Make sure it is BELOW the location ~ \.php(?:$|/) { block
          (nameValuePair "~* \.(?:css|js)$" ''
            add_header Cache-Control "public, max-age=7200";
            # Add headers to serve security related headers
            add_header Strict-Transport-Security "max-age=15768000; includeSubDomains; preload;";
            add_header X-Content-Type-Options nosniff;
            add_header X-Frame-Options "SAMEORIGIN";
            add_header X-XSS-Protection "1; mode=block";
            add_header X-Robots-Tag none;
            # Optional: Don't log access to assets
            access_log off;
          '')

          # Optional: Don't log access to other assets
          (nameValuePair "~* \.(?:jpg|jpeg|gif|bmp|ico|png|swf)$" ''
            access_log off;
          '')
        ];
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

  serveWordpress = domains:
    let
      domain = head domains;

    in {
      krebs.nginx.servers."${domain}" = {
        server-names = domains;
        extraConfig = ''
          root /srv/http/${domain}/;
          index index.php;
          access_log /tmp/nginx_acc.log;
          error_log /tmp/nginx_err.log;
          error_page 404 /404.html;
          error_page 500 502 503 504 /50x.html;
        '';
        locations = [
          (nameValuePair "/" ''
            try_files $uri $uri/ /index.php?$args;
          '')
          (nameValuePair "~ \.php$" ''
            fastcgi_pass unix:/srv/http/${domain}/phpfpm.pool;
            include ${pkgs.nginx}/conf/fastcgi.conf;
          '')
          (nameValuePair "~ /\\." ''
            deny all;
          '')
          #Directives to send expires headers and turn off 404 error logging.
          (nameValuePair "~* ^.+\.(xml|ogg|ogv|svg|svgz|eot|otf|woff|mp4|ttf|css|rss|atom|js|jpg|jpeg|gif|png|ico|zip|tgz|gz|rar|bz2|doc|xls|exe|ppt|tar|mid|midi|wav|bmp|rtf)$" ''
            access_log off;
            log_not_found off;
            expires max;
          '')
        ];
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

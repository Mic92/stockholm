{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  cfg = config.lass.wordpress;

  out = {
    options.lass.wordpress = api;
    config = imp;
  };

  api = mkOption {
    type = with types; attrsOf (submodule ({ config, ... }: {
      options = {
        domain = mkOption {
          type = str;
          default = config._module.args.name;
        };
        dbUser = mkOption {
          type = str;
          default = replaceStrings ["."] ["_"] config.domain;
        };
        dbName = mkOption {
          type = str;
          default = replaceStrings ["."] ["_"] config.domain;
        };
        folder = mkOption {
          type = str;
          default = "/srv/http/${config.domain}";
        };
        auto = mkOption {
          type = bool;
          default = false;
        };
        charset = mkOption {
          type = str;
          default = "utf8mb4";
        };
        collate = mkOption {
          type = str;
          default = "";
        };
        debug = mkOption {
          type = bool;
          default = false;
        };
        multiSite = mkOption {
          type = attrsOf str;
          default = {};
          example = {
            "0" = "bla.testsite.de";
            "1" = "test.testsite.de";
          };
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
              ciphers = mkOption {
                type = str;
                default = "AES128+EECDH:AES128+EDH";
              };
            };
          });
        };
      };
    }));
    default = {};
  };

  user = config.services.nginx.user;
  group = config.services.nginx.group;

  imp = {
    #services.nginx.appendConfig = mkIf (cfg.multiSite != {}) ''
    #  map $http_host $blogid {
    #  ${concatStringsSep "\n" (mapAttrsToList (n: v: indent "v n;") multiSite)}
    #  }
    #'';

    krebs.nginx.servers = flip mapAttrs cfg ( name: { domain, folder, multiSite, ssl, ... }: {
      server-names = [
        "${domain}"
        "www.${domain}"
      ];
        #(mkIf (multiSite != {})
        #)
      locations = (if (multiSite != {}) then
        [
          (nameValuePair "~ ^/files/(.*)$" ''
            try_files /wp-content/blogs.dir/$blogid/$uri /wp-includes/ms-files.php?file=$1 ;
          '')
          (nameValuePair "^~ /blogs.dir" ''
            internal;
            alias ${folder}/wp-content/blogs.dir ;
            access_log off; log_not_found off; expires max;
          '')
        ]
      else
        []
      ) ++
      [
        (nameValuePair "/" ''
          try_files $uri $uri/ /index.php?$args;
        '')
        (nameValuePair "~ \.php$" ''
          fastcgi_pass unix:${folder}/phpfpm.pool;
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
      extraConfig = ''
        root ${folder}/;
        index index.php;
        access_log /tmp/nginx_acc.log;
        error_log /tmp/nginx_err.log;
        error_page 404 /404.html;
        error_page 500 502 503 504 /50x.html;
        ${if ssl.enable then ''
          ssl_certificate ${ssl.certificate};
          ssl_certificate_key ${ssl.certificate_key};
        '' else ""}

      '';
      listen = (if ssl.enable then
          [ "80" "443 ssl" ]
        else
          "80"
      );
    });
    services.phpfpm.poolConfigs = flip mapAttrs cfg (name: { domain, folder, ... }: ''
      listen = ${folder}/phpfpm.pool
      user = ${user}
      group = ${group}
      pm = dynamic
      pm.max_children = 5
      pm.start_servers = 2
      pm.min_spare_servers = 1
      pm.max_spare_servers = 3
      listen.owner = ${user}
      listen.group = ${group}
      # errors to journal
      php_admin_value[error_log] = 'stderr'
      php_admin_flag[log_errors] = on
      catch_workers_output = yes
    '');
    systemd.services = flip mapAttrs' cfg (name: { domain, folder, charset, collate, dbName, dbUser, debug, multiSite, ... }: {
      name = "wordpressInit-${name}";
      value = {
        path = [
          pkgs.mysql
          pkgs.su
          pkgs.gawk
          pkgs.jq
        ];
        requiredBy = [ "nginx.service" ];
        serviceConfig = let
          php.define = name: value:
            "define(${php.newdoc name}, ${php.newdoc value});";
          php.toString = x:
            "'${x}'";
          php.newdoc = s:
            let b = "EOF${builtins.hashString "sha256" s}"; in
            ''<<<'${b}'
            ${s}
            ${b}
            '';
        in {
          Type = "oneshot";
          ExecStart = pkgs.writeScript "wordpressInit" ''
            #!/bin/sh
            set -euf
            wp_secrets=${shell.escape "${toString <secrets>}/${domain}/wp-secrets"}
            db_password=$(cat ${shell.escape "${toString <secrets>}/${domain}/sql-db-pw"})
            get_secret() {
              echo "define('$1', $(jq -r ."$1" "$wp_secrets" | to_php_string));"
            }
            to_php_string() {
              echo "base64_decode('$(base64)')"
            }
            {
              cat ${toString <secrets/mysql_rootPassword>}
              password=$(cat ${shell.escape (toString (<secrets/mysql_rootPassword>))})
              # TODO passwordhash=$(su nobody2 -c mysql <<< "SELECT PASSWORD($(toSqlString <<< "$password"));")
              # TODO as package pkgs.sqlHashPassword
              # TODO not using mysql
              # SET SESSION sql_mode = 'NO_BACKSLASH_ESCAPES';
              passwordhash=$(su nobody2 -c 'mysql -u nobody --silent' <<< "SELECT PASSWORD('$db_password');")
              user=${shell.escape dbUser}@localhost
              database=${shell.escape dbName}
              cat << EOF
                CREATE DATABASE IF NOT EXISTS $database;
                GRANT USAGE ON *.* TO $user IDENTIFIED BY PASSWORD '$passwordhash';
                GRANT ALL PRIVILEGES ON $database.* TO $user;
                FLUSH PRIVILEGES;
            EOF
            } | mysql -u root -p
            # TODO nix2php for wp-config.php
            cat > ${folder}/wp-config.php << EOF
            <?php
            define('DB_PASSWORD', '$db_password');
            define('DB_HOST', 'localhost');

            ${concatStringsSep "\n" (mapAttrsToList (name: value:
              "define('${name}', $(printf '%s' ${shell.escape value} | to_php_string));"
            ) {
              DB_NAME = dbName;
              DB_USER = dbUser;
              DB_CHARSET = charset;
              DB_COLLATE = collate;
            })}

            ${concatMapStringsSep "\n" (key: "$(get_secret ${shell.escape key})") [
              "AUTH_KEY"
              "SECURE_AUTH_KEY"
              "LOGGED_IN_KEY"
              "NONCE_KEY"
              "AUTH_SALT"
              "SECURE_AUTH_SALT"
              "LOGGED_IN_SALT"
              "NONCE_SALT"
            ]}

            \$table_prefix = 'wp_';

            ${if (multiSite != {}) then
              "define('WP_ALLOW_MULTISITE', true);"
            else
              ""
            }

            define('WP_DEBUG', ${toJSON debug});
            if ( !defined('ABSPATH') )
              define('ABSPATH', dirname(__FILE__) . '/');

            /** Sets up WordPress vars and included files. */
            require_once(ABSPATH . 'wp-settings.php');
            EOF
          '';
        };
      };
    });
    users.users.nobody2 = mkDefault {
      uid = mkDefault (genid "nobody2");
      useDefaultShell = mkDefault true;
    };
  };

  indent = replaceChars ["\n"] ["\n  "];

in out

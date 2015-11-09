{ config, lib, pkgs, ... }:

with lib;

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
      };
    }));
    default = {};
  };

  dataFolder = "/srv/http";
  user = config.services.nginx.user;
  group = config.services.nginx.group;

  imp = {
    krebs.nginx.servers = flip mapAttrs cfg ( name: { domain, ... }: {
      server-names = [
        "${domain}"
        "www.${domain}"
      ];
      locations = [
        (nameValuePair "/" ''
          try_files $uri $uri/ /index.php?$args;
        '')
        (nameValuePair "~ \.php$" ''
          fastcgi_pass unix:${dataFolder}/${domain}/phpfpm.pool;
          include ${pkgs.nginx}/conf/fastcgi.conf;
        '')
        (nameValuePair "~ /\\." ''
          deny all;
        '')
      ];
      extraConfig = ''
        root ${dataFolder}/${domain}/;
        index index.php;
        access_log /tmp/nginx_acc.log;
        error_log /tmp/nginx_err.log;
        error_page 404 /404.html;
        error_page 500 502 503 504 /50x.html;
      '';
    });
    services.phpfpm.poolConfigs = flip mapAttrs cfg (name: { domain, ... }: ''
      listen = ${dataFolder}/${domain}/phpfpm.pool
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
    systemd.services = flip mapAttrs' cfg (name: { domain, folder, charset, collate, dbName, dbUser, debug, ... }: {
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
    users.users.nobody2 = {
      uid = 125816384; # genid nobody2
      useDefaultShell = true;
    };
  };

in out

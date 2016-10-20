{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.lass.owncloud;

  out = {
    options.lass.owncloud = api;
    config = imp;
  };

  api = mkOption {
    type = with types; attrsOf (submodule ({ config, ... }: {
      options = {
        domain = mkOption {
          type = str;
          default = config._module.args.name;
        };
        dataDir = mkOption {
          type = str;
          default = "${config.folder}/data";
        };
        dbUser = mkOption {
          type = str;
          default = replaceStrings ["."] ["_"] config.domain;
        };
        dbName = mkOption {
          type = str;
          default = replaceStrings ["."] ["_"] config.domain;
        };
        dbType = mkOption {
        # TODO: check for valid dbType
          type = str;
          default = "mysql";
        };
        folder = mkOption {
          type = str;
          default = "/srv/http/${config.domain}";
        };
        auto = mkOption {
          type = bool;
          default = false;
        };
        instanceid = mkOption {
          type = str;
        };
      };
    }));
    default = {};
  };

  user = config.services.nginx.user;
  group = config.services.nginx.group;

  imp = {
    krebs.nginx.servers = flip mapAttrs cfg ( name: { domain, folder, ... }: {
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
          fastcgi_pass unix:${folder}/phpfpm.pool;
        '')
        (nameValuePair "~ /\\." ''
          deny all;
        '')
      ];
      extraConfig = ''
        root ${folder}/;
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
      php_admin_value[error_log] = 'stderr'
      php_admin_flag[log_errors] = on
      catch_workers_output = yes
    '');
    #systemd.services = flip mapAttrs' cfg (name: { domain, folder, dbName, dbUser, dbType, dataDir, instanceid, ... }: {
    #  name = "owncloudInit-${name}";
    #  value = {
    #    path = [
    #      pkgs.mysql
    #      pkgs.su
    #      pkgs.gawk
    #      pkgs.jq
    #    ];
    #    requiredBy = [ "nginx.service" ];
    #    serviceConfig = let
    #      php.define = name: value:
    #        "define(${php.newdoc name}, ${php.newdoc value});";
    #      php.toString = x:
    #        "'${x}'";
    #      php.newdoc = s:
    #        let b = "EOF${builtins.hashString "sha256" s}"; in
    #        ''<<<'${b}'
    #        ${s}
    #        ${b}
    #        '';
    #    in {
    #      Type = "oneshot";
    #      ExecStart = pkgs.writeScript "wordpressInit" ''
    #        #!/bin/sh
    #        set -euf
    #        oc_secrets=${shell.escape "${toString <secrets>}/${domain}/oc-secrets"}
    #        db_password=$(cat ${shell.escape "${toString <secrets>}/${domain}/sql-db-pw"})
    #        get_secret() {
    #          echo "'$1' => $(jq -r ."$1" "$oc_secrets" | to_php_string),"
    #        }
    #        to_php_string() {
    #          echo "base64_decode('$(base64)')"
    #        }
    #        {
    #          cat ${toString <secrets/mysql_rootPassword>}
    #          password=$(cat ${shell.escape (toString (<secrets/mysql_rootPassword>))})
    #          # TODO passwordhash=$(su nobody_oc -c mysql <<< "SELECT PASSWORD($(toSqlString <<< "$password"));")
    #          # TODO as package pkgs.sqlHashPassword
    #          # TODO not using mysql
    #          # SET SESSION sql_mode = 'NO_BACKSLASH_ESCAPES';
    #          passwordhash=$(su nobody_oc -c 'mysql -u nobody --silent' <<< "SELECT PASSWORD('$db_password');")
    #          user=${shell.escape dbUser}@localhost
    #          database=${shell.escape dbName}
    #          cat << EOF
    #            CREATE DATABASE IF NOT EXISTS $database;
    #            GRANT USAGE ON *.* TO $user IDENTIFIED BY PASSWORD '$passwordhash';
    #            GRANT ALL PRIVILEGES ON $database.* TO $user;
    #            FLUSH PRIVILEGES;
    #        EOF
    #        } | mysql -u root -p
    #        # TODO nix2php for wp-config.php
    #        mkdir -p ${folder}/config
    #        cat > ${folder}/config/config.php << EOF
    #        <?php
    #        \$CONFIG = array (
    #          'dbhost' => 'localhost',
    #          'dbtableprefix' => 'oc_',
    #          'dbpassword' => '$db_password',
    #          'installed' => 'true',
    #          'trusted_domains' =>
    #          array (
    #            0 => '${domain}',
    #          ),
    #          'overwrite.cli.url' => 'http://${domain}',

    #        ${concatStringsSep "\n" (mapAttrsToList (name: value:
    #          "'${name}' => $(printf '%s' ${shell.escape value} | to_php_string),"
    #        ) {
    #          instanceid = instanceid;
    #          datadirectory = dataDir;
    #          dbtype = dbType;
    #          dbname = dbName;
    #          dbuser = dbUser;
    #        })}

    #        ${concatMapStringsSep "\n" (key: "$(get_secret ${shell.escape key})") [
    #          "secret"
    #          "passwordsalt"
    #        ]}
    #        );
    #        EOF
    #      '';
    #    };
    #  };
    #});
    users.users.nobody_oc = {
      uid = genid "nobody_oc";
      useDefaultShell = true;
    };
  };

in out

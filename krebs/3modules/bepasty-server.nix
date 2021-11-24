{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  gunicorn = pkgs.python3Packages.gunicorn;
  bepasty = pkgs.bepasty;
  gevent = pkgs.python3Packages.gevent;
  python = pkgs.python3Packages.python;
  cfg = config.krebs.bepasty;

  out = {
    options.krebs.bepasty = api;
    config = lib.mkIf cfg.enable (lib.mkMerge [
      (lib.mkIf cfg.serveNginx nginx-imp)
      imp
    ]);
  };

  api = {
    enable = mkEnableOption "Bepasty Servers";
    serveNginx = mkEnableOption "Serve Bepasty Servers with Nginx";

    servers = mkOption {
      type = with types; attrsOf optionSet;
      example = ''
        {
          "paste.r" = {
            defaultPermissions = "read,delete,create";
          };
          "paste.krebsco.de" = {
            defaultPermissions = "read";
          };
        }
      '';
      options = singleton {
        nginx = mkOption {
          # TODO use the correct type
          type = with types; attrsOf unspecified;
          description = ''
            Additional nginx configuration.
          '';
        };
        secretKey = mkOption {
          type = types.str;
          description = ''
            server secret for safe session cookies, must be set.
          '';
          default = "";
        };


        # we create a wsgi socket in $workDir/gunicorn-${name}.wsgi
        workDir = mkOption {
          type = types.str;
          description = ''
            Path to the working directory (used for sockets and pidfile).
            Defaults to the users home directory. Must be accessible to nginx,
            permissions will be set to 755
          '';
          default = config.users.extraUsers.bepasty.home;
          defaultText = "<literal>\${config.users.extraUsers.bepasty.home}</literal>";
        };

        dataDir = mkOption {
          type = types.str;
          description = ''
            Defaults to the new users home dir which defaults to
            /var/lib/bepasty-server/data
          '';
          default = "${config.users.extraUsers.bepasty.home}/data";
          defaultText = "<literal>\${config.users.extraUsers.bepasty.home}/data</literal>";
        };

        extraConfig = mkOption {
          type = types.str;
          default = "";
          # TODO configure permissions in separate
          example = ''
            PERMISSIONS = {
              'myadminsecret': 'admin,list,create,read,delete',
            }
            MAX_ALLOWED_FILE_SIZE = 5 * 1000 * 1000
          '';
        };

        defaultPermissions = mkOption {
          # TODO: listOf str
          type = types.str;
          description = ''
            default permissions for all unauthenticated users.
          '';
          example = "read,create,delete";
          default = "read";
        };

      };
      default = {};
    };

  };

  imp = {
    # Configures systemd services for each configured server
    # environment.systemPackages = [ bepasty gunicorn gevent ];
    systemd.services = mapAttrs' (name: server:
      nameValuePair "bepasty-server-${name}" {
        description = "Bepasty Server ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        environment = let
          penv = python.buildEnv.override {
            extraLibs = [ bepasty gevent ];
          };
        in {
          BEPASTY_CONFIG = "${server.workDir}/bepasty-${name}.conf";
          PYTHONPATH= "${penv}/${python.sitePackages}/";
        };

        serviceConfig = {
          Type = "simple";
          PrivateTmp = true;

          ExecStartPre = assert server.secretKey != ""; pkgs.writeDash "bepasty-server.${name}-init" ''
            mkdir -p "${server.dataDir}" "${server.workDir}"
            chown bepasty:bepasty "${server.workDir}" "${server.dataDir}"
            cat > "${server.workDir}/bepasty-${name}.conf" <<EOF
            SITENAME="${name}"
            STORAGE_FILESYSTEM_DIRECTORY="${server.dataDir}"
            SECRET_KEY="${server.secretKey}"
            DEFAULT_PERMISSIONS="${server.defaultPermissions}"
            ${server.extraConfig}
            EOF
          '';
          ExecStart = ''${gunicorn}/bin/gunicorn bepasty.wsgi --name ${name} \
            -u bepasty \
            -g bepasty \
            --workers 3 --log-level=info \
            --bind=unix:${server.workDir}/gunicorn-${name}.sock \
            --pid ${server.workDir}/gunicorn-${name}.pid \
            -k gevent
          '';
        };
      }
    ) cfg.servers;

    users.extraUsers.bepasty = {
      uid = genid_uint31 "bepasty";
      group = "bepasty";
      home = "/var/lib/bepasty-server";
      isSystemUser = true;
    };
    users.extraGroups.bepasty = {
      gid = genid_uint31 "bepasty";
    };
  };

  nginx-imp = {
    assertions = [{ assertion = config.services.nginx.enable;
                     message = "services.nginx.enable must be true"; }];

    services.nginx.virtualHosts = mapAttrs ( name: server:
      (mkMerge [
        server.nginx
        {
          extraConfig = ''
            client_max_body_size 32M;
            '';
          locations = {
            "/".extraConfig = "proxy_set_header Host $host;";
            "/".proxyPass = "http://unix:${server.workDir}/gunicorn-${name}.sock";
            "/static/".extraConfig = ''
              alias ${bepasty}/lib/${python.libPrefix}/site-packages/bepasty/static/;
            '';
          };
        }])
      ) cfg.servers ;
  };
in
out

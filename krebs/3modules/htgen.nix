{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.htgen;

  out = {
    options.krebs.htgen = api;
    config = imp;
  };

  api = mkOption {
    default = {};
    type = types.attrsOf (types.submodule ({ config, ... }: {
      options = {
        enable = mkEnableOption "krebs.htgen-${config.name}";

        name = mkOption {
          type = types.username;
          default = config._module.args.name;
        };

        package = mkOption {
          default = pkgs.htgen;
          type = types.package;
        };

        port = mkOption {
          type = types.uint;
        };

        script = mkOption {
          type = types.str;
        };
        user = mkOption {
          type = types.user;
          default = {
            name = "htgen-${config.name}";
            home = "/var/lib/htgen-${config.name}";
          };
        };
      };
    }));
  };
  imp = {

    systemd.services = mapAttrs' (name: htgen:
      nameValuePair "htgen-${name}" {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        environment = {
          HTGEN_PORT = toString htgen.port;
          HTGEN_SCRIPT = htgen.script;
        };
        serviceConfig = {
          SyslogIdentifier = "htgen";
          User = htgen.user.name;
          PrivateTmp = true;
          Restart = "always";
          ExecStart = "${htgen.package}/bin/htgen --serve";
        };
      }
    ) cfg;

    users.users = mapAttrs' (name: htgen:
      nameValuePair htgen.user.name {
        inherit (htgen.user) home name uid;
        createHome = true;
        isSystemUser = true;
      }
    ) cfg;

  };
in out

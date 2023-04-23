{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  cfg = config.krebs.konsens;

  out = {
    options.krebs.konsens = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "git konsens finder";
    repos = mkOption {
      type = types.attrsOf (types.submodule ({ config, ...}: {
        options = {
          url = mkOption {
            type = types.str;
            default = "git@localhost:${config._module.args.name}";
          };
          branchesToCheck = mkOption {
            type = types.listOf types.str;
            default = [ "lassulus" "makefu" "tv" ];
          };
          target = mkOption {
            type = types.str;
            default = "master";
          };
          timerConfig = mkOption {
            type = types.attrsOf types.str;
            default = {
              OnCalendar = "*:00,15,30,45";
            };
          };
        };
      }));
    };
  };

  imp = {
    users.groups.konsens.gid = genid "konsens";
    users.users.konsens = rec {
      name = "konsens";
      group = "konsens";
      uid = genid name;
      home = "/var/lib/konsens";
      isSystemUser = true;
      createHome = true;
    };

    systemd.timers = mapAttrs' (name: repo:
      nameValuePair "konsens-${name}" {
        description = "konsens timer";
        wantedBy = [ "timers.target" ];
        timerConfig = repo.timerConfig;
      }
    ) cfg.repos;

    systemd.services = mapAttrs' (name: repo:
      nameValuePair "konsens-${name}" {
        after = [ "network.target" ];
        path = [ pkgs.git ];
        restartIfChanged = false;
        serviceConfig = {
          Type = "simple";
          PermissionsStartOnly = true;
          ExecStart = pkgs.writeDash "konsens-${name}" ''
            if ! test -e ${name}; then
              git clone ${repo.url} ${name}
            fi
            cd ${name}
            git fetch origin
            git push origin $(git merge-base --octopus ${concatMapStringsSep " " (branch: "origin/${branch}") repo.branchesToCheck}):refs/heads/master
          '';
          WorkingDirectory = /var/lib/konsens;
          User = "konsens";
        };
      }
    ) cfg.repos;
  };

in out

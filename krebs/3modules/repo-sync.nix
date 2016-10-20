{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.repo-sync;

  out = {
    options.krebs.repo-sync = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "repo-sync";
    repos = mkOption {
      type = with types;attrsOf (attrsOf (attrsOf (attrsOf str)));
      example = literalExample ''
        # see `repo-sync --help`
        #   `ref` provides sane defaults and can be omitted

        # you can have multiple repo-sync groups and therefore multiple @latest
        # configuration entries.
        # attrset will be converted to json and be used as config
        # each attrset defines a group of repos for syncing

        { nxpkgs = {
            makefu = {
              origin = {
                url = http://github.com/makefu/nixpkgs;
                ref = "heads/dev" ;
              };
              mirror = {
                url = "git@internal:nixpkgs-mirror" ;
                ref = "heads/github-mirror-dev" ;
              };
            };
            lass = {
              origin = {
                url = http://github.com/lass/nixpkgs;
              };
              mirror = {
                url = "git@internal:nixpkgs-mirror" ;
              };
            };
            "@latest" = {
              mirror = {
                url = "git@internal:nixpkgs-mirror";
                ref = "heads/master";
              };
            };
          };
          stockholm = {
            lass = {
              origin = {
                url = http://cgit.prism.r/stockholm;
              };
              mirror = {
                url = "git@internal:stockholm-mirror" ;
              };
            };
            makefu = {
              origin = {
                url = http://gum.krebsco.de/stockholm;
              };
              mirror = {
                url = "git@internal:stockholm-mirror" ;
              };
            };
            "@latest" = {
              mirror = {
                url = "git@internal:stockholm-mirror";
                ref = "heads/master";
              };
            };
          };
        };
      '';
    };
    timerConfig = mkOption {
      type = types.attrsOf types.str;
      default = {
        OnCalendar = "*:00,15,30,45";
      };
    };
    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/repo-sync";
    };

    user = mkOption {
      type = types.user;
      default = {
        name = "repo-sync";
        home = cfg.stateDir;
      };
    };

    privateKeyFile = mkOption {
      type = types.secret-file;
      default = {
        path = "${cfg.stateDir}/ssh.priv";
        owner = cfg.user;
        source-path = toString <secrets> + "/repo-sync.ssh.key";
      };
    };

    unitConfig = mkOption {
      type = types.attrsOf types.str;
      description = "Extra unit configuration for fetchWallpaper to define conditions and assertions for the unit";
      example = literalExample ''
        # do not start when running on umts
        { ConditionPathExists = "!/var/run/ppp0.pid"; }
      '';
      default = {};
    };

  };

  imp = {
    krebs.secret.files.repo-sync-key = cfg.privateKeyFile;
    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
      description = "repo-sync user";
    };

    systemd.timers = mapAttrs' (name: repo:
      nameValuePair "repo-sync-${name}" {
        description = "repo-sync timer";
        wantedBy = [ "timers.target" ];

        timerConfig = cfg.timerConfig;
      }
    ) cfg.repos;

    systemd.services = mapAttrs' (name: repo:
      let
        repo-sync-config = pkgs.writeText "repo-sync-config-${name}.json"
          (builtins.toJSON repo);
      in nameValuePair "repo-sync-${name}" {
        description = "repo-sync";
        after = [ "network.target" "secret.service" ];

        environment = {
          GIT_SSH_COMMAND = "${pkgs.openssh}/bin/ssh -i ${cfg.stateDir}/ssh.priv";
          REPONAME = "${name}.git";
        };

        serviceConfig = {
          Type = "simple";
          PermissionsStartOnly = true;
          ExecStart = "${pkgs.repo-sync}/bin/repo-sync ${repo-sync-config}";
          WorkingDirectory = cfg.stateDir;
          User = "repo-sync";
        };
        unitConfig = cfg.unitConfig;
      }
    ) cfg.repos;
  };
in out

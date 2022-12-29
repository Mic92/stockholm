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
      type = types.attrsOf (types.submodule {
        options = {
          branches = mkOption {
            type = types.attrsOf (types.submodule ({ config, ... }: {
              options = {
                origin = mkOption {
                  type = types.source-types.git;
                };
                mirror = mkOption {
                  type = types.source-types.git;
                };
              };
              config = {
                origin.ref = mkDefault "heads/master";
                mirror.ref = mkDefault "heads/${config._module.args.name}";
              };
            }));
          };
          latest = mkOption {
            type = types.nullOr types.source-types.git;
            default = null;
          };
          timerConfig = mkOption {
            type = types.attrsOf types.str;
            default = cfg.timerConfig;
          };
        };
      });
      example = literalExample ''
        # see `repo-sync --help`
        #   `ref` provides sane defaults and can be omitted

        # you can have multiple repo-sync groups and therefore multiple @latest
        # configuration entries.
        # attrset will be converted to json and be used as config
        # each attrset defines a group of repos for syncing

        { nxpkgs = {
            branches = {
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
            };
            latest = {
              url = "git@internal:nixpkgs-mirror";
              ref = "heads/master";
            };
          };
          stockholm = {
            branches = {
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
            };
            latest = {
              url = "git@internal:stockholm-mirror";
              ref = "heads/master";
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
      type = types.absolute-pathname;
      default = toString <secrets> + "/repo-sync.ssh.key";
      defaultText = "‹secrets/repo-sync.ssh.key›";
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
    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
      group = cfg.user.name;
      description = "repo-sync user";
      isSystemUser = true;
    };

    users.groups.${cfg.user.name} = {};

    systemd.timers = mapAttrs' (name: repo:
      nameValuePair "repo-sync-${name}" {
        description = "repo-sync timer";
        wantedBy = [ "timers.target" ];
        timerConfig = repo.timerConfig;
      }
    ) cfg.repos;

    krebs.systemd.services = mapAttrs' (name: _:
      nameValuePair "repo-sync-${name}" {
        restartIfCredentialsChange = true;
      }
    ) cfg.repos;

    systemd.services = mapAttrs' (name: repo:
      let
        repo-sync-config = pkgs.writeJSON "repo-sync-config-${name}.json"
          (repo.branches // optionalAttrs (repo.latest != null) {
            "@latest".mirror = repo.latest;
          });
      in nameValuePair "repo-sync-${name}" {
        description = "repo-sync";
        after = [ "network.target" ];

        environment = {
          GIT_SSH_COMMAND = "${pkgs.openssh}/bin/ssh -i $CREDENTIALS_DIRECTORY/ssh_key";
          REPONAME = "${name}.git";
        };

        restartIfChanged = false;
        serviceConfig = {
          Type = "simple";
          PermissionsStartOnly = true;
          LoadCredential = "ssh_key:${cfg.privateKeyFile}";
          ExecStart = "${pkgs.repo-sync}/bin/repo-sync ${repo-sync-config}";
          WorkingDirectory = cfg.stateDir;
          User = "repo-sync";
        };
        unitConfig = cfg.unitConfig;
      }
    ) cfg.repos;
  };
in out

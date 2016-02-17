{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.krebs.repo-sync;

  out = {
    options.krebs.repo-sync = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "repo-sync";
    config = mkOption {
      type = with types;attrsOf (attrsOf (attrsOf str));
      example = literalExample ''
        # see `repo-sync --help`
        #   `ref` provides sane defaults and can be omitted

        # attrset will be converted to json and be used as config
        {
            makefu = {
                origin = {
                    url = http://github.com/makefu/repo ;
                    ref = "heads/dev" ;
                };
                mirror = {
                    url = "git@internal:mirror" ;
                    ref = "heads/github-mirror-dev" ;
                };
            };
            lass = {
                origin = {
                    url = http://github.com/lass/repo ;
                };
                mirror = {
                    url = "git@internal:mirror" ;
                };
            };
            "@latest" = {
                mirror = {
                    url = "git@internal:mirror";
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
    privateKeyFile = mkOption {
      type = types.str;
      description = ''
        used by repo-sync to identify with ssh service
      '';
      default = toString <secrets/wolf-repo-sync.rsa_key.priv>;
    };
  };
  repo-sync-config = pkgs.writeText "repo-sync-config.json"
    (builtins.toJSON cfg.config);

  imp = {
    users.users.repo-sync = {
      name = "repo-sync";
      uid = config.krebs.lib.genid "repo-sync";
      description = "repo-sync user";
      home = cfg.stateDir;
      createHome = true;
    };

    systemd.timers.repo-sync = {
      description = "repo-sync timer";
      wantedBy = [ "timers.target" ];

      timerConfig = cfg.timerConfig;
    };
    systemd.services.repo-sync = {
      description = "repo-sync";
      after = [ "network.target" ];

      path = with pkgs; [ ];

      environment = {
        GIT_SSH_COMMAND = "${pkgs.openssh}/bin/ssh -i ${cfg.stateDir}/ssh.priv";
      };

      serviceConfig = {
        Type = "simple";
        PermissionsStartOnly = true;
        ExecStartPre = pkgs.writeScript "prepare-repo-sync-user" ''
          #! /bin/sh
          cp -v ${config.krebs.lib.shell.escape cfg.privateKeyFile} ${cfg.stateDir}/ssh.priv
          chown repo-sync ${cfg.stateDir}/ssh.priv
        '';
        ExecStart = "${pkgs.repo-sync}/bin/repo-sync ${repo-sync-config}";
        WorkingDirectory = cfg.stateDir;
        User = "repo-sync";
      };
    };
  };
in out

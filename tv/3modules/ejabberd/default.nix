{ config, lib, pkgs, ... }@args: with import <stockholm/lib>; let

  cfg = config.tv.ejabberd;

  gen-dhparam = pkgs.writeDash "gen-dhparam" ''
    set -efu
    path=$1
    bits=2048
    # TODO regenerate dhfile after some time?
    if ! test -e "$path"; then
      ${pkgs.openssl}/bin/openssl dhparam "$bits" > "$path"
    fi
  '';

in {
  options.tv.ejabberd = {
    enable = mkEnableOption "tv.ejabberd";
    certfile = mkOption {
      type = types.secret-file;
      default = {
        name = "ejabberd-certfile";
        path = "${cfg.user.home}/ejabberd.pem";
        owner = cfg.user;
        source-path = toString <secrets> + "/ejabberd.pem";
      };
    };
    dhfile = mkOption {
      type = types.secret-file;
      default = {
        name = "ejabberd-dhfile";
        path = "${cfg.user.home}/dhparams.pem";
        owner = cfg.user;
        source-path = "/dev/null";
      };
    };
    hosts = mkOption {
      type = with types; listOf str;
    };
    pkgs.ejabberd = mkOption {
      type = types.package;
      default = pkgs.symlinkJoin {
        name = "ejabberd-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            exec ${pkgs.ejabberd}/bin/ejabberdctl \
                --config ${toFile "ejabberd.yaml" (import ./config.nix {
                  inherit pkgs;
                  config = cfg;
                })} \
                --logs ${shell.escape cfg.user.home} \
                --spool ${shell.escape cfg.user.home} \
                "$@"
          '')
          pkgs.ejabberd
        ];
      };
    };
    registration_watchers = mkOption {
      type = types.listOf types.str;
      default = [
        config.krebs.users.tv.mail
      ];
    };
    s2s_certfile = mkOption {
      type = types.secret-file;
      default = cfg.certfile;
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "ejabberd";
        home = "/var/lib/ejabberd";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.symlinkJoin {
        name = "ejabberd-sudo-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            set -efu
            cd ${shell.escape cfg.user.home}
            exec /run/wrappers/bin/sudo \
                -u ${shell.escape cfg.user.name} \
                ${cfg.pkgs.ejabberd}/bin/ejabberdctl "$@"
          '')
          cfg.pkgs.ejabberd
        ];
      })
    ];

    krebs.secret.files = {
      ejabberd-certfile = cfg.certfile;
      ejabberd-s2s_certfile = cfg.s2s_certfile;
    };

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [
        config.krebs.secret.files.ejabberd-certfile.service
        config.krebs.secret.files.ejabberd-s2s_certfile.service
        "network.target"
      ];
      partOf = [
        config.krebs.secret.files.ejabberd-certfile.service
        config.krebs.secret.files.ejabberd-s2s_certfile.service
      ];
      serviceConfig = {
        ExecStartPre = "${gen-dhparam} ${cfg.dhfile.path}";
        ExecStart = "${cfg.pkgs.ejabberd}/bin/ejabberdctl foreground";
        PermissionsStartOnly = true;
        SyslogIdentifier = "ejabberd";
        User = cfg.user.name;
        TimeoutStartSec = 60;
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
      group = cfg.user.name;
      isSystemUser = true;
    };

    users.groups.${cfg.user.name} = {};
  };
}

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
    certfiles = mkOption {
      type = types.listOf types.absolute-pathname;
      default = [
        (toString <secrets> + "/ejabberd.pem")
      ];
    };
    credentials.certfiles = mkOption {
      internal = true;
      readOnly = true;
      default =
        imap
          (i: const /* yaml */ "/tmp/credentials/certfile${toJSON i}")
          cfg.certfiles;
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
                --ctl-config ${toFile "ejabberdctl.cfg" /* sh */ ''
                  ERL_OPTIONS='-setcookie ${cfg.stateDir}/.erlang.cookie'
                ''} \
                --logs ${cfg.stateDir} \
                --spool ${cfg.stateDir} \
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
    stateDir = mkOption {
      type = types.absolute-pathname;
      default = "/var/lib/ejabberd";
      readOnly = true;
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.symlinkJoin {
        name = "ejabberd-sudo-wrapper";
        paths = [
          (pkgs.writeDashBin "ejabberdctl" ''
            exec ${pkgs.systemd}/bin/systemd-run \
                --unit=ejabberdctl \
                --property=StateDirectory=ejabberd \
                --property=User=ejabberd \
                --collect \
                --pipe \
                --quiet \
                ${cfg.pkgs.ejabberd}/bin/ejabberdctl "$@"
          '')
          cfg.pkgs.ejabberd
        ];
      })
    ];

    krebs.systemd.services.ejabberd = {};

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStartPre = [
          "${pkgs.coreutils}/bin/ln -s \${CREDENTIALS_DIRECTORY} /tmp/credentials"
          "${gen-dhparam} ${cfg.stateDir}/dhfile"
        ];
        ExecStart = "${cfg.pkgs.ejabberd}/bin/ejabberdctl foreground";
        ExecStop = [
          "${cfg.pkgs.ejabberd}/bin/ejabberdctl stop"
          "${cfg.pkgs.ejabberd}/bin/ejabberdctl stopped"
        ];
        ExecReload = "${cfg.pkgs.ejabberd}/bin/ejabberdctl reload_config";
        LoadCredential =
          zipListsWith
            (dst: src: "${baseNameOf dst}:${src}")
            cfg.credentials.certfiles
            cfg.certfiles;
        LimitNOFILE = 65536;
        PrivateDevices = true;
        PrivateTmp = true;
        SyslogIdentifier = "ejabberd";
        StateDirectory = "ejabberd";
        User = "ejabberd";
        DynamicUser = true;
        TimeoutSec = 60;
        RestartSec = 5;
        Restart = "on-failure";
        Type = "notify";
        NotifyAccess = "all";
        WatchdogSec = 30;
      };
    };
  };
}

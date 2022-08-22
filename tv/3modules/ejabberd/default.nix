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
      type = types.absolute-pathname;
      default = toString <secrets> + "/ejabberd.pem";
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
        ExecStart = pkgs.writeDash "ejabberd" ''
          ${pkgs.coreutils}/bin/ln -s "$CREDENTIALS_DIRECTORY" /tmp/credentials
          ${gen-dhparam} ${cfg.stateDir}/dhfile
          exec ${cfg.pkgs.ejabberd}/bin/ejabberdctl foreground
        '';
        LoadCredential = [
          "certfile:${cfg.certfile}"
        ];
        PrivateTmp = true;
        SyslogIdentifier = "ejabberd";
        StateDirectory = "ejabberd";
        User = "ejabberd";
        DynamicUser = true;
        TimeoutStartSec = 60;
      };
    };
  };
}

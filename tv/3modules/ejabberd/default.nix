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
        path = "${cfg.user.home}/ejabberd.pem";
        owner = cfg.user;
        source-path = toString <secrets> + "/ejabberd.pem";
      };
    };
    dhfile = mkOption {
      type = types.secret-file;
      default = {
        path = "${cfg.user.home}/dhparams.pem";
        owner = cfg.user;
        source-path = "/dev/null";
      };
    };
    hosts = mkOption {
      type = with types; listOf str;
    };
    pkgs.ejabberdctl = mkOption {
      type = types.package;
      default = pkgs.writeDashBin "ejabberdctl" ''
        exec ${pkgs.ejabberd}/bin/ejabberdctl \
            --config ${toFile "ejabberd.yaml" (import ./config.nix {
              inherit pkgs;
              config = cfg;
            })} \
            --logs ${shell.escape cfg.user.home} \
            --spool ${shell.escape cfg.user.home} \
            "$@"
      '';
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
        home = "/var/ejabberd";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.pkgs.ejabberdctl ];

    krebs.secret.files = {
      ejabberd-certfile = cfg.certfile;
      ejabberd-s2s_certfile = cfg.s2s_certfile;
    };

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "secret.service" ];
      after = [ "network.target" "secret.service" ];
      serviceConfig = {
        ExecStartPre = "${gen-dhparam} ${cfg.dhfile.path}";
        ExecStart = "${cfg.pkgs.ejabberdctl}/bin/ejabberdctl foreground";
        PermissionsStartOnly = true;
        SyslogIdentifier = "ejabberd";
        User = cfg.user.name;
        TimeoutStartSec = 60;
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name uid;
      createHome = true;
    };
  };
}

{ config, lib, pkgs, ... }@args: with config.krebs.lib; let
  cfg = config.tv.ejabberd;
in {
  options.tv.ejabberd = {
    enable = mkEnableOption "tv.ejabberd";
    certfile = mkOption {
      type = types.secret-file;
      default = {
        path = "${cfg.user.home}/ejabberd.pem";
        owner-name = "ejabberd";
        source-path = toString <secrets> + "/ejabberd.pem";
      };
    };
    hosts = mkOption {
      type = with types; listOf str;
    };
    pkgs.ejabberdctl = mkOption {
      type = types.package;
      default = pkgs.writeDashBin "ejabberdctl" ''
        set -efu
        export SPOOLDIR=${shell.escape cfg.user.home}
        export EJABBERD_CONFIG_PATH=${shell.escape (import ./config.nix args)}
        exec ${pkgs.ejabberd}/bin/ejabberdctl \
            --logs ${shell.escape cfg.user.home} \
            "$@"
      '';
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
        Type = "oneshot";
        RemainAfterExit = "yes";
        PermissionsStartOnly = "true";
        SyslogIdentifier = "ejabberd";
        User = cfg.user.name;
        ExecStart = "${cfg.pkgs.ejabberdctl}/bin/ejabberdctl start";
      };
    };

    users.users.${cfg.user.name} = {
      inherit (cfg.user) home name;
      createHome = true;
      uid = genid cfg.user.name;
    };
  };
}

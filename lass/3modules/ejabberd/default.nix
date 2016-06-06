{ config, lib, pkgs, ... }@args: with config.krebs.lib; let
  cfg = config.lass.ejabberd;
in {
  options.lass.ejabberd = {
    enable = mkEnableOption "lass.ejabberd";
    certfile = mkOption {
      type = types.str;
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
            --spool ${shell.escape cfg.user.home} \
            "$@"
      '';
    };
    s2s_certfile = mkOption {
      type = types.str;
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

    systemd.services.ejabberd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
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
      inherit (cfg.user) home name uid;
      createHome = true;
    };
  };
}

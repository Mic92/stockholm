{ config, lib, pkgs, ... }@args: with import <stockholm/lib>; let
  cfg = config.tv.charybdis;
in {
  options.tv.charybdis = {
    enable = mkEnableOption "tv.charybdis";
    motd = mkOption {
      type = types.str;
      default = "/join #retiolum";
    };
    port = mkOption {
      type = types.int;
      default = 6667;
    };
    ssl_cert = mkOption {
      type = types.path;
    };
    ssl_dh_params = mkOption {
      type = types.secret-file;
      default = {
        name = "charybdis-ssl_dh_params";
        path = "${cfg.user.home}/dh.pem";
        owner = cfg.user;
        source-path = toString <secrets> + "/charybdis.dh.pem";
      };
    };
    ssl_private_key = mkOption {
      type = types.secret-file;
      default = {
        name = "charybdis-ssl_private_key";
        path = "${cfg.user.home}/ssl.key.pem";
        owner = cfg.user;
        source-path = toString <secrets> + "/charybdis.key.pem";
      };
    };
    sslport = mkOption {
      type = types.int;
      default = 6697;
    };
    user = mkOption {
      type = types.user;
      default = {
        name = "charybdis";
        home = "/var/lib/charybdis";
      };
    };
  };
  config = lib.mkIf cfg.enable {

    krebs.secret.files.charybdis-ssl_dh_params = cfg.ssl_dh_params;
    krebs.secret.files.charybdis-ssl_private_key = cfg.ssl_private_key;

    environment.etc."charybdis-ircd.motd".text = cfg.motd;

    systemd.services.charybdis = {
      wantedBy = [ "multi-user.target" ];
      after = [
        config.krebs.secret.files.charybdis-ssl_dh_params.service
        config.krebs.secret.files.charybdis-ssl_private_key.service
        "network-online.target"
      ];
      partOf = [
        config.krebs.secret.files.charybdis-ssl_dh_params.service
        config.krebs.secret.files.charybdis-ssl_private_key.service
      ];
      environment = {
        BANDB_DBPATH = "${cfg.user.home}/ban.db";
      };
      serviceConfig = {
        SyslogIdentifier = "charybdis";
        User = cfg.user.name;
        PrivateTmp = true;
        Restart = "always";
        ExecStartPre =
          "${pkgs.coreutils}/bin/ln -s /etc/charybdis-ircd.motd /tmp/ircd.motd";
        ExecStart = toString [
          "${pkgs.charybdis}/bin/charybdis"
            "-configfile ${import ./config.nix args}"
            "-foreground"
            "-logfile /dev/stderr"
        ];
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

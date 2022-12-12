with import ./lib;
{ config, pkgs, ... }@args: let
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
      type = types.absolute-pathname;
      default = toString <secrets> + "/charybdis.dh.pem";
    };
    ssl_private_key = mkOption {
      type = types.absolute-pathname;
      default = toString <secrets> + "/charybdis.key.pem";
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

    environment.etc."charybdis-ircd.motd".text = cfg.motd;

    krebs.systemd.services.charybdis = {};

    systemd.services.charybdis = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      environment = {
        BANDB_DBPATH = "${cfg.user.home}/ban.db";
      };
      serviceConfig = {
        SyslogIdentifier = "charybdis";
        User = cfg.user.name;
        PrivateTmp = true;
        Restart = "always";
        ExecStartPre = [
          "${pkgs.coreutils}/bin/ln -s /etc/charybdis-ircd.motd /tmp/ircd.motd"
          "${pkgs.coreutils}/bin/ln -s \${CREDENTIALS_DIRECTORY} /tmp/credentials"
        ];
        ExecStart = toString [
          "${pkgs.charybdis}/bin/charybdis"
            "-configfile ${import ./config.nix args}"
            "-foreground"
            "-logfile /dev/stderr"
        ];
        LoadCredential = [
          "ssl_dh_params:${cfg.ssl_dh_params}"
          "ssl_private_key:${cfg.ssl_private_key}"
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
